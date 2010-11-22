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
    [gloss.core protocols]
    [gloss.data primitives])
  (:require
    [gloss.data.bytes :as bytes]
    [gloss.core.formats :as formats]
    [gloss.data.string :as string]
    [gloss.core.codecs :as codecs]
    [gloss.core.structure :as structure]))

;;;

(import-fn #'structure/compile-frame)

(defmacro defcodec [name frame]
  `(def ~name (compile-frame ~frame)))

;;;

(import-fn #'formats/to-byte-buffer)
(import-fn #'formats/to-buf-seq)

(defn contiguous
  "Takes a sequence of ByteBuffers and returns a single contiguous ByteBuffer."
  [buf-seq]
  (bytes/take-contiguous-bytes (bytes/byte-count buf-seq) buf-seq))

(defn encode
  "Turns a frame value into a sequence of ByteBuffers."
  [codec val]
  (write-bytes codec nil val))

(defn encode-all
  "Turns a sequence of frame values into a sequence of ByteBuffers."
  [codec vals]
  (apply concat
    (map #(write-bytes codec nil %) vals)))

(defn decode
  "Turns bytes into a single frame value.  If there are too few or too many bytes
   for the frame, an exception is thrown."
  [codec bytes]
  (let [buf-seq (bytes/dup-bytes (to-buf-seq bytes))
	[success val remainder] (read-bytes codec buf-seq)]
    (when-not success
      (throw (Exception. "Insufficient bytes to decode frame.")))
    (when-not (empty? remainder)
      (throw (Exception. "Bytes left over after decoding frame.")))
    val))

(defn decode-all
  "Turns bytes into a sequence of frame values.  If there are bytes left over at the end
   of the sequence, an exception is thrown."
  [codec bytes]
  (let [buf-seq (bytes/dup-bytes (to-buf-seq bytes))]
    (loop [buf-seq buf-seq, vals []]
      (if (empty? buf-seq)
	vals
	(let [[success val remainder] (read-bytes codec buf-seq)]
	  (when-not success
	    (throw (Exception. "Bytes left over after decoding sequence of frames.")))
	  (recur remainder (conj vals val)))))))

;;;

(import-fn codecs/enum)

(defn delimited-block
  [delimiters strip-delimiters?]
  (bytes/delimited-bytes-codec delimiters strip-delimiters?))

(defn finite-block
  [len]
  (bytes/finite-byte-codec len))

(defn delimited-frame
  [delimiters frame]
  (bytes/delimited-codec delimiters (compile-frame frame)))

(defn finite-frame
  [prefix-or-len frame]
  (bytes/wrap-finite-block
    (if (number? prefix-or-len)
      (codecs/constant-prefix prefix-or-len)
      (compile-frame prefix-or-len))
    (compile-frame frame)))

(defn string
  [charset & {:as options}]
  (let [charset (name charset)]
    (cond
      (:length options)
      (string/finite-string-codec charset (:length options))

      (:delimiters options)
      (bytes/delimited-codec
	(string/string-codec charset)
	(map to-byte-buffer (:delimiters options)))

      :else
      (string/string-codec charset))))

(defn header [frame header->body body->header]
  (codecs/header
    (compile-frame frame)
    header->body
    body->header))

;;;

(defn prefix
  ([primitive]
     (prefix primitive identity identity))
  ([signature to-integer from-integer]
     (codecs/prefix (compile-frame signature) to-integer from-integer)))

(defn repeated [frame & {:as options}]
  (let [codec (compile-frame frame)]
    (cond
      (:delimiters options)
      (bytes/wrap-delimited-sequence
	codec
	(map to-byte-buffer (:delimiters options)))
      
      :else
      (codecs/wrap-prefixed-sequence
	(or (:prefix options) (:int32 primitive-codecs))
	codec))))
