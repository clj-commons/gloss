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
    [gloss.core formats])
  (:import
    [java.nio Buffer ByteBuffer]
    [gloss.data.bytes.core SingleBufferSequence]))

;;;

(defprotocol Reader
  (read-bytes [this buf-seq bounded?]))

(defprotocol Writer
  (sizeof [this])
  (write-bytes [this buf val]))

(defn reader? [x]
  (satisfies? Reader x))

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
      (let [[success x xs] (read-bytes reader (to-buf-seq buf-seq) false)]
	(if success
	  (recur (conj result x) xs)
	  result)))))

(defn compose-callback [codec callback]
  (reify
    Reader
    (read-bytes [this buf-seq bounded?]
      (let [[success x bytes] (read-bytes codec buf-seq bounded?)]
	(if success
	  (callback x bytes bounded?)
	  [false (compose-callback x callback) (to-buf-seq bytes)])))
    Writer
    (sizeof [_]
      (sizeof codec))
    (write-bytes [_ buf val]
      (throw (Exception. "write-bytes not supported")))))

(defn compose-readers [a b]
  (reify
    Reader
    (read-bytes [this buf-seq bounded?]
      (let [[success x bytes] (read-bytes a buf-seq bounded?)]
	(if success
	  (read-bytes
	    (compose-callback
	      b
	      (fn [v remainder _]
		(assert (empty? remainder))
		[true v bytes]))
	    x
	    bounded?)
	  [false (compose-readers x b) (to-buf-seq bytes)])))
    Writer
    (sizeof [_]
      nil)
    (write-bytes [_ buf val]
      (throw (Exception. "write-bytes not supported")))))

(defn take-all [codec]
  (fn [buf-seq remainder _]
    (loop [bytes (to-buf-seq buf-seq), vals []]
      (if (empty? bytes)
	[true vals remainder]
	(let [[success v b] (read-bytes codec bytes false)]
	  (when-not success
	    (throw (Exception. "Cannot evenly divide bytes into sequence of frames.")))
	  (recur b (conj vals v)))))))

;;;

