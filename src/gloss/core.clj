;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns gloss.core
  (:use
    potemkin
    [gloss.core.protocols :exclude (sizeof)]
    [gloss.data primitives]
    [gloss.core.formats :only (to-byte-buffer to-buf-seq)])
  (:require
    [gloss.core.protocols :as protocols]
    [gloss.data.bytes :as bytes]
    [gloss.core.formats :as formats]
    [gloss.data.string :as string]
    [gloss.core.codecs :as codecs]
    [gloss.core.structure :as structure]))

;;;

(import-fn #'structure/compile-frame)

(defmacro defcodec
  "Defines a compiled frame."
  [name frame]
  `(def ~name (compile-frame ~frame)))

(defmacro defcodec-
  "Defines a private compiled frame."
  [name frame]
  `(defcodec ^{:private true} ~name ~frame))

(import-fn #'protocols/sizeof)

(defn byte-count
  "Returns the number of bytes in the value.  Compatible with any data structure that can
   be transformed into a sequence of ByteBuffers."
  [b]
  (bytes/byte-count (to-buf-seq b)))

;;;

(import-fn codecs/enum)
(import-fn codecs/ordered-map)

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
   such as :utf-8 or :ascii.  Available options are :length, :delimiters, and :as-str.

   A string with :length specified is of finite size:

   (string :utf-8 :length 3)

   A string with :delimiters specified is terminated by one or more delimiters:

   (string :utf-8 :delimiters [\"\\r\\n\" \"\\r\"])

   By default, this function decodes to a java.lang.CharSequence instead of an
   actual string.  This is more memory-efficient, and clojure.contrib.str operates
   on CharSequences.  However, if a real string is necessary, set :as-str to true:

   (string :utf-8, :length 3, :as-str true)"
  [charset & {:as options}]
  (let [charset (name charset)]
    (compile-frame
      (cond
	(:length options)
	(string/finite-string-codec charset (:length options))
	
	(:delimiters options)
	(bytes/delimited-codec
	  (->> (:delimiters options)
	    (map #(if (string? %) (.getBytes ^String % charset) %))
	    (map to-byte-buffer))
	  (string/string-codec charset))
	
	:else
	(string/string-codec charset))
      identity
      (if (or (:as-str options) false)
	str
	identity))))

(defn string-integer
  [charset & {:as options}]
  (let [codec (apply string charset (apply concat options))
	read-codec (compose-callback
		     codec
		     (fn [n b _]
		       [true (Long/parseLong (str n)) b]))]
    (reify
      Reader
      (read-bytes [_ b bounded?]
	(read-bytes read-codec b bounded?))
      Writer
      (sizeof [_]
	nil)
      (write-bytes [_ _ v]
	(write-bytes codec nil (str (long v)))))))

(defn string-float
  [charset & {:as options}]
  (let [codec (apply string charset (apply concat options))
	read-codec (compose-callback
		     codec
		     (fn [n b _]
		       [true (Double/parseDouble (str n)) b]))]
    (reify
      Reader
      (read-bytes [_ b bounded?]
	(read-bytes read-codec b bounded?))
      Writer
      (sizeof [_]
	nil)
      (write-bytes [_ _ v]
	(write-bytes codec nil (str (double v)))))))

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
	codec)

      (= :none (:prefix options))
      (let [reader (take-all codec)]
	(reify
	  Reader
	  (read-bytes [_ b _]
	    (reader b nil true))
	  Writer
	  (sizeof [_]
	    nil)
	  (write-bytes [_ buf vs]
	    (if-let [size (sizeof codec)]
	      (with-buffer [buf (* (count vs) size)]
		(doseq [v vs]
		  (write-bytes codec buf v)))
	      (apply concat
		(map #(write-bytes codec nil %) vs))))))
      
      :else
      (codecs/wrap-prefixed-sequence
	(or (compile-frame (:prefix options)) (:int32 primitive-codecs))
	codec))))
