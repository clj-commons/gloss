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
    [gloss.data primitives string])
  (:require
    [gloss.data.bytes :as bytes]
    [gloss.core.formats :as formats]
    [gloss.core.frame :as frame]))

;;;

(import-fn #'frame/compile-frame)

(defmacro def-frame [name frame]
  `(def ~name (compile-frame ~frame)))

;;;

(import-fn #'formats/to-byte-buffer)

(defn contiguous
  "Takes a sequence of ByteBuffers and returns a single contiguous ByteBuffer."
  [buf-seq]
  (bytes/take-contiguous-bytes (bytes/buf-seq-count buf-seq) buf-seq))

(defn encode
  "Turns a frame value into a sequence of ByteBuffers."
  [frame val]
  (write-bytes frame nil val))

(defn encode-all
  "Turns a sequence of frame values into a sequence of ByteBuffers."
  [frame vals]
  (apply concat
    (map #(write-bytes frame nil %) vals)))

(defn decode
  "Turns bytes into a single frame value.  If there are too few or too many bytes
   for the frame, an exception is thrown."
  [frame bytes]
  (let [buf-seq (bytes/dup-buf-seq (formats/to-buf-seq bytes))
	[success val remainder] (read-bytes frame buf-seq)]
    (when-not success
      (throw (Exception. "Insufficient bytes to decode frame.")))
    (when-not (empty? remainder)
      (throw (Exception. "Bytes left over after decoding frame.")))
    val))

(defn decode-all
  "Turns bytes into a sequence of frame values.  If there are bytes left over at the end
   of the sequence, an exception is thrown."
  [frame bytes]
  (let [buf-seq (bytes/dup-buf-seq (formats/to-buf-seq bytes))]
    (loop [buf-seq buf-seq, vals []]
      (if (empty? buf-seq)
	vals
	(let [[success val remainder] (read-bytes frame buf-seq)]
	  (when-not success
	    (throw (Exception. "Bytes left over after decoding sequence of frames.")))
	  (recur remainder (conj vals val)))))))

;;;


(defn delimited-block
  [delimiters frame]
  (bytes/delimited-block delimiters (compile-frame frame)))

(defn string
  [charset & {:as options}]
  (let [charset (name charset)]
    (cond
      (:length options)
      (finite-string-codec charset (:length options))

      (:delimiters options)
      (bytes/delimited-block
	(string-codec charset)
	(map to-byte-buffer (:delimiters options)))

      :else
      (string-codec charset))))

(defn header [frame header->body body->header]
  (frame/header
    (compile-frame frame)
    header->body
    body->header))

;;;

(defn enum [& map-or-seq]
  (let [n->v (if (and (= 1 (count map-or-seq)) (map? (first map-or-seq)))
	       (let [m (first map-or-seq)]
		 (zipmap
		   (map short (vals m))
		   (keys m)))
	       (zipmap
		 (map short (range (count map-or-seq)))
		 map-or-seq))
	v->n (zipmap (vals n->v) (keys n->v))
	codec (:int16 primitive-codecs)]
    (reify
      Reader
      (read-bytes [this b]
	(let [[success x b] (read-bytes codec b)]
	  (if success
	    [true (n->v (short x)) b]
	    [false this b])))
      Writer
      (sizeof [_]
	(sizeof codec))
      (write-bytes [_ buf v]
	(write-bytes codec buf (v->n v))))))

(defn prefix
  ([primitive]
     (prefix primitive identity identity))
  ([signature to-integer from-integer]
     (frame/prefix (compile-frame signature) to-integer from-integer)))

(defn repeated [frame & {:as options}]
  (let [codec (compile-frame frame)]
    (cond
      (:delimiters options)
      (bytes/wrap-delimited-sequence
	codec
	(map to-byte-buffer (:delimiters options)))
      
      :else
      (frame/wrap-prefixed-sequence
	(or (:prefix options) (:int32 primitive-codecs))
	codec))))
