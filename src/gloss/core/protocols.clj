;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns gloss.core.protocols
  (:import [java.nio Buffer ByteBuffer]))

(defprotocol Reader
  (read-bytes [this buf-seq]))

(defprotocol UnboundedWriter
  (create-buf [this val]))

(defprotocol BoundedWriter
  (size [this])
  (write-to-buf [this buf val]))

(defn reader? [x]
  (satisfies? Reader x))

(defn bounded-writer? [x]
  (satisfies? BoundedWriter x))

(defn unbounded-writer? [x]
  (satisfies? UnboundedWriter x))

(defn writer? [x]
  (or (bounded-writer? x) (unbounded-writer? x)))

(defn write-bytes [codec val]
  (cond
    (bounded-writer? codec)
    (let [buf (ByteBuffer/allocate (size codec))]
      (write-to-buf codec buf val)
      [(.rewind ^ByteBuffer buf)])

    (unbounded-writer? codec)
    (create-buf codec val)))

(def byte-array-class (class (byte-array [])))

(defn to-buf-seq [x]
  (when x
    (cond
      (and (sequential? x) (or (empty? x) (instance? ByteBuffer (first x)))) x
      (= (class x) byte-array-class) [(ByteBuffer/wrap x)]
      (instance? ByteBuffer x) [x]
      :else (throw (Exception. (str "Cannot convert to buf-seq: " x))))))

(defn frame-seq [reader buf-seq]
  (loop [result [], buf-seq buf-seq]
    (let [[x xs] (read-bytes reader buf-seq)]
      (if-not (reader? x)
	(recur (conj result x) xs)
	result))))

(defn read-comp [codec callback]
  (reify
    Reader
    (read-bytes [this buf-seq]
      (let [[x bytes :as result] (read-bytes codec buf-seq)]
	(if (reader? x)
	  [this bytes]
	  (callback result))))
    UnboundedWriter
    (create-buf [this val]
      (write-bytes codec val))))
