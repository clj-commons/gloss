;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns gloss.core.protocols
  (:use [gloss.core formats])
  (:import [java.nio Buffer ByteBuffer]))

;;;

(defprotocol Reader
  (read-bytes [this buf-seq]))

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
       [(.rewind ^Buffer ~buf)])))

;;;

(defn frame-seq [reader buf-seq]
  (loop [result [], buf-seq buf-seq]
    (if (empty? buf-seq)
      result
      (let [[success x xs] (read-bytes reader buf-seq)]
	(if success
	  (recur (conj result x) xs)
	  result)))))

(defn- read-callback [codec callback]
  (reify
    Reader
    (read-bytes [this buf-seq]
      (let [[success x bytes] (read-bytes codec buf-seq)]
	(if success
	  (callback x bytes)
	  [false (read-callback x callback) bytes])))
    Writer
    (sizeof [_]
      (sizeof codec))
    (write-bytes [_ buf val]
      (throw (Exception. "write-bytes not supported")))))

(defn compose-readers [codec & readers]
  (let [readers (map
		  (fn [rd]
		    (if (reader? rd)
		      #(apply read-bytes rd %)
		      rd))
		  readers)]
    (reduce read-callback codec readers)))

;;;

