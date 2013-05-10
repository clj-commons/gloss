;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^{:skip-wiki true}
  gloss.core.protocols
  (:use
	[potemkin]
    [gloss.core formats])
  (:import
    [java.nio Buffer ByteBuffer]
    [gloss.data.bytes.core SingleBufferSequence]))

;;;

(defprotocol+ Reader
  (read-bytes [this buf-seq]
     "Attempt to decode the given sequence of byte buffers. Returns a 3-tuple [success x remainder].
     If success is truthy, then x is the decoded value and remainder is whatever is left after
     decoding x.

     A falsey success indicates that there are not enough bytes to decode a complete value. In this
     case, x is a codec that can be used to continue decoding, and remainder is data that still must
     be given to that codec before any further bytes. For example, suppose the codec [:int32 :int32]
     is given the five bytes [00 00 00 20 FF]. It might reasonably read the first integer (32), then
     return as its remainder [FF] and as its codec something like (compile-frame [:int32] identity
     #(cons 32 %)), thus closing around the values it can process ahead of time.

     If, rather than being incomplete, the bytes to be read are faulty in some way, an exception
     will be thrown."))

(defprotocol+ Writer
  (sizeof [this]
    "Returns the number of bytes this codec will encode to, or nil if it is value-dependent.")
  (write-bytes [this buf val]))

(defn reader? [x]
  (satisfies? Reader x))

(def ^{:dynamic true} complete? false)
(def ^{:dynamic true} trailing? false)

;;;

(defmacro with-buffer [[buf size] & body]
  `(if ~buf
     (do ~@body)
     (let [~buf (ByteBuffer/allocate ~size)]
       (do ~@body)
       (SingleBufferSequence. (.rewind ^Buffer ~buf) ~size))))

;;;

(defn frame-seq [reader buf-seq]
  (loop [result [], buf-seq buf-seq]
    (if (empty? buf-seq)
      result
      (let [[success x xs] (read-bytes reader (to-buf-seq buf-seq))]
	(if success
	  (recur (conj result x) xs)
	  result)))))

(defn compose-callback [codec callback]
  (reify
    Reader
    (read-bytes [this buf-seq]
      (let [[success x bytes] (read-bytes codec buf-seq)]
	(if success
	  (callback x bytes)
	  [false (compose-callback x callback) (to-buf-seq bytes)])))
    Writer
    (sizeof [_]
      (sizeof codec))
    (write-bytes [_ buf val]
      (throw (Exception. "write-bytes not supported")))))

(defn compose-readers [a b]
  (reify
    Reader
    (read-bytes [this buf-seq]
      (let [[success x bytes] (read-bytes a buf-seq)]
	(if success
	  (read-bytes
	    (compose-callback
	      b
	      (fn [v remainder]
		(assert (empty? remainder))
		[true v bytes]))
	    x)
	  [false (compose-readers x b) (to-buf-seq bytes)])))
    Writer
    (sizeof [_]
      nil)
    (write-bytes [_ buf val]
      (throw (Exception. "write-bytes not supported")))))

(defn take-all [codec]
  (fn [buf-seq remainder]
    (loop [bytes (to-buf-seq buf-seq), vals []]
      (if (empty? bytes)
	[true vals remainder]
	(let [[success v b] (read-bytes codec bytes)]
	  (when-not success
	    (throw (Exception. "Cannot evenly divide bytes into sequence of frames.")))
	  (recur b (conj vals v)))))))

;;;

