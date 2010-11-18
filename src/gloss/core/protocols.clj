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
    (let [[success x xs] (read-bytes reader buf-seq)]
      (if success
	(recur (conj result x) xs)
	result))))

(defn- read-callback [codec callback]
  (reify
    Reader
    (read-bytes [this buf-seq]
      (let [[success x bytes] (read-bytes codec buf-seq)]
	(if success
	  (callback x bytes)
	  [this bytes])))
    Writer
    (sizeof [_] (sizeof codec))
    (write-bytes [_ buf val] (write-bytes codec buf val))))

(defn compose-readers [codec & readers]
  (let [readers (map
		  (fn [rd]
		    (if (reader? rd)
		      #(apply read-bytes rd %)
		      rd))
		  readers)]
    (reduce read-callback codec readers)))

;;;

(defn header [sig header->body body->header]
  (let [codec (compose-readers
		sig
		(fn [v b]
		  (let [body (header->body v)]
		    (read-bytes body b))))]
    (reify
      Reader
      (read-bytes [_ buf-seq]
	(read-bytes codec buf-seq))
      Writer
      (sizeof [_]
	nil)
      (write-bytes [_ buf val]
	(let [header (body->header val)]
	  (concat
	    (write-bytes sig buf (body->header val))
	    (write-bytes (header->body header) buf val)))))))

;;;

(declare read-sequence)

(defn- sequence-reader [codec len vals]
  (reify
    Reader
    (read-bytes [_ buf-seq]
      (read-sequence codec buf-seq len vals))))

(defn read-sequence [codec buf-seq len vals]
  (loop [buf-seq buf-seq, vals vals]
    (if (= (count vals) len)
      [true vals buf-seq]
      (let [[success x b] (read-bytes codec buf-seq)]
	(if success
	  (recur b (conj vals x))
	  [false (sequence-reader codec len vals) b])))))

(defn wrap-prefixed-sequence
  [prefix-codec codec]
  (assert (sizeof prefix-codec))
  (let [codec* (compose-readers
		 prefix-codec
		 (fn [len b]
		   (read-sequence codec b len [])))
	sizeof-prefix (sizeof prefix-codec)]
    (reify
      Reader
      (read-bytes [_ b]
	(read-bytes codec* b))
      Writer
      (write-bytes [_ buf vs]
	(let [cnt (count vs)]
	  (if (sizeof codec)
	    (with-buffer [buf (+ sizeof-prefix (* cnt (sizeof codec)))]
	      (write-bytes prefix-codec buf cnt)
	      (doseq [v vs]
		(write-bytes codec buf v)))
	    (concat
	      (with-buffer [buf sizeof-prefix]
		(write-bytes prefix-codec buf cnt))
	      (apply concat
		(map #(write-bytes codec buf %) vs)))))))))
