;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns gloss.core
  (:use
    [gloss.core.protocols :exclude [sizeof]])
  (:require
    [gloss.core.formats :refer [to-byte-buffer to-buf-seq string-to-byte-buffer]]
    [gloss.core.protocols :as protocols]
    [gloss.data.bytes :as bytes]
    [gloss.data.primitives :refer :all]
    [gloss.core.formats :as formats]
    [gloss.data.string :as string]
    [gloss.core.codecs :as codecs]
    [gloss.core.structure :as structure]
    [potemkin :refer :all]))

;;;

(import-fn structure/compile-frame)

(defmacro defcodec
  "Defines a compiled frame."
  [name frame & coders]
  `(def ~name (compile-frame ~frame ~@coders)))

(defmacro defcodec-
  "Defines a private compiled frame."
  [name frame & coders]
  `(defcodec ~(with-meta name (assoc (meta name) :private true)) ~frame ~@coders))

(import-fn protocols/sizeof)

(defn byte-count
  "Returns the number of bytes in the value.  Compatible with any data structure that can
   be transformed into a sequence of ByteBuffers."
  [b]
  (bytes/byte-count (to-buf-seq b)))

;;;

(import-fn bytes/bit-seq)

(defn bit-map
  "Defines an ordered map of signed integers with the specified bit-lengths.  The sum of
   the bit-lengths must be divisible by 8.

   (bit-map :a 3, :b 4, :c 1) <=> {:a 3, :b -15, :c false}"
  [& args]
  (let [ks (map first (partition 2 args))
        vs (map second (partition 2 args))]
    (compile-frame (apply bit-seq vs)
                   (fn [val] (map #(get val %) ks))
                   #(zipmap ks %))))

;;;

(import-fn codecs/enum)
(import-fn codecs/ordered-map)

(def nil-frame
  (reify
    Reader
    (read-bytes [_ b]
      [true nil b])
    Writer
    (sizeof [_]
      nil)
    (write-bytes [_ _ v]
      )))

(defn delimited-block
  "Defines a frame which is just a byte sequence terminated by one or more delimiters.

   If strip-delimiters? is true, the resulting byte sequences will not contain the
   delimiters."
  [delimiters strip-delimiters?]
  (bytes/delimited-bytes-codec
    (map to-byte-buffer delimiters)
    strip-delimiters?))

(defn finite-block
  "Defines a byte sequence which is either of fixed length or has a prefix which
   describes its length."
  [prefix-or-len]
  (if (number? prefix-or-len)
    (bytes/finite-byte-codec prefix-or-len)
    (bytes/wrap-finite-block (compile-frame prefix-or-len) codecs/identity-codec)))

(defn delimited-frame
  "Defines a frame which is terminated by delimiters."
  [delimiters frame]
  (bytes/delimited-codec
    (map to-byte-buffer delimiters)
    (compile-frame frame)))

(defn finite-frame
  "Defines a frame which is either of finite length, or has a prefix which describes
   its length."
  [prefix-or-len frame]
  (bytes/wrap-finite-block
    (if (number? prefix-or-len)
      (codecs/constant-prefix prefix-or-len)
      (compile-frame prefix-or-len))
    (compile-frame frame)))


(defn string
  "Defines a frame which contains a string.  The charset must be a keyword,
   such as :utf-8 or :ascii.  Available options are :length, :delimiters, :suffix,
   and :char-sequence.

   A string with :length specified is of finite byte length:

   (string :ascii :length 3)

   A string with :delimiters specified is terminated by one or more delimiters:

   (string :utf-8 :delimiters [\"\\r\\n\" \"\\r\"])

   By specifying :value->delimiter you can selectively delimit a value when encoding:

   (string :utf-8 :delimiters [\"\\r\\n\" \" \"] :value->delimiter (fn [v] (if (= v \"END\")  [\"\\r\\n\"] [\" \"])))

   If a string is already bounded in length, but has a terminating sequence, use :suffix

   (string :utf-8, :length 3, :suffix \"\\r\\n\")

   :suffix can be used in conjunction with both :length and :delimiters.  The given :length
   is assumed to not include the suffix, and the delimiters are assumed to be followed by the
   suffix.

   By default, this frame will return a string, but it can be more memory-efficient to
   have it return a java.lang.CharSequence instead.  This frame also will encode CharSequences.
   To have the decoded result be a CharSequence, set :char-sequence to true:

   (string :utf-8, :length 3, :char-sequence true)"
  [charset & {:as options}]
  (let [options (merge
                  {:char-sequence false}
                  options)
        charset (name charset)
        suffix-length (if (:suffix options)
                        (-> options :suffix to-byte-buffer to-buf-seq byte-count)
                        0)]
    (codecs/wrap-suffixed-codec
      (:suffix options)
      (compile-frame
        (cond
          (or (:length options) (:prefix options))
          (finite-frame (or (:length options) (:prefix options))
                        (string/string-codec charset))

          (:delimiters options)
          (let [delimiters (:delimiters options)
                bytes->delimiter (if (:value->delimiter options)
                                   (:value->delimiter options)
                                   (fn [v] delimiters))]

            (bytes/delimited-codec
              (string-to-byte-buffer (:delimiters options) charset)
              (get options :strip-delimiters? true)
              (string/string-codec charset)
              #(string-to-byte-buffer (bytes->delimiter %) charset)))

          :else
          (string/string-codec charset))
        identity
        (if-not (:char-sequence options)
          str
          identity)))))

(defn- pad-number [s options]
  (if-not (:length options)
    s
    (let [lo (:length options)
          ls (count s)]
      (cond
        (< ls lo) (str (apply str (repeat (- lo ls) "0")) s)
        (> ls lo) (throw (Exception. (str "'" s "' is longer than given length of " lo)))
        :else s))))

(defn string-integer
  [charset & {:as options}]
  (let [codec (apply string charset (apply concat options))
        read-codec (compose-callback
                     codec
                     (fn [n b]
                       [true (Long/parseLong (str n)) b]))]
    (reify
      Reader
      (read-bytes [_ b]
        (read-bytes read-codec b))
      Writer
      (sizeof [_]
        nil)
      (write-bytes [_ _ v]
        (write-bytes codec nil (pad-number (str (long v)) options))))))

(defn string-float
  [charset & {:as options}]
  (let [codec (apply string charset (apply concat options))
        read-codec (compose-callback
                     codec
                     (fn [n b]
                       [true (Double/parseDouble (str n)) b]))]
    (reify
      Reader
      (read-bytes [_ b]
        (read-bytes read-codec b))
      Writer
      (sizeof [_]
        nil)
      (write-bytes [_ _ v]
        (write-bytes codec nil (pad-number (str (double v)) options))))))

(defn header
  "A header is a frame which describes the frame that follows.  The decoded value
   from the header frame will be passed into 'header->body,' which will return the
   resulting codec for the body.  When encoding, the value of the body will be passed
   into 'body->header,' which will return the resulting value for the header."
  [frame header->body body->header]
  (codecs/header
    (compile-frame frame)
    header->body
    body->header))

;;;

(defn prefix
  "A prefix is a specialized form of header, which only describes the length of the sequence
   that follows.  It is only meant to be used in the context of 'finite-frame' or 'repeated'.
   A prefix may be as simple as a primitive type:

   (prefix :int32)

   But may also be a more complex frame:

   (prefix
     [(string :utf-8 :delimiters [\"\\0\"]) :int64]
     second
     (fn [n] [\"hello\" n]))

   For complex prefixes, 'to-count' must take the value of the header and return the length
   of the sequence that follows, and 'from-count' must take the length of the sequence and
   return the value of the prefix."
  ([primitive]
   (prefix primitive identity identity))
  ([signature to-count from-count]
   (codecs/prefix (compile-frame signature) to-count from-count)))

(defn repeated
  "Describes a sequence of frames.  By default, the sequence is prefixed with a 32-bit integer
   describing the length of the sequence.  However, specifying a custom :prefix or :delimiters that
   terminate the sequence is also allowed."
  [frame & {:as options}]
  (let [codec (compile-frame frame)]
    (cond
      (:delimiters options)
      (bytes/wrap-delimited-sequence
        (map to-byte-buffer (:delimiters options))
        (get options :strip-delimiters? true)
        codec)

      (= :none (:prefix options))
      (let [reader (take-all codec)]
        (reify
          Reader
          (read-bytes [_ b]
            (if (zero? (bytes/byte-count b))
              [true [] nil]
              (reader b nil)))
          Writer
          (sizeof [_]
            nil)
          (write-bytes [_ buf vs]
            (if-let [size (sizeof codec)]
              (with-buffer [buf (* (count vs) size)]
                           (doseq [v vs]
                             (write-bytes codec buf v)))
              (doall
                (apply concat
                       (map #(write-bytes codec buf %) vs)))))))

      :else
      (codecs/wrap-prefixed-sequence
        (or (compile-frame (:prefix options)) (:int32 primitive-codecs))
        codec))))
