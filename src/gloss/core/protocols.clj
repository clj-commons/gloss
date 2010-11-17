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

(defprotocol UnboundedWriter
  (create-buf [this val]))

(defprotocol BoundedWriter
  (sizeof [this val])
  (write-to-buf [this buf val]))

(defn reader? [x]
  (satisfies? Reader x))

(defn bounded-writer? [x]
  (satisfies? BoundedWriter x))

(defn unbounded-writer? [x]
  (satisfies? UnboundedWriter x))

(defn writer? [x]
  (or (bounded-writer? x) (unbounded-writer? x)))

;;;

(def *current-buffer* nil)

(defn write-bytes [codec val]
  (cond
    (bounded-writer? codec)
    (if *current-buffer*
      (write-to-buf codec *current-buffer* val)
      (let [buf (ByteBuffer/allocate (sizeof codec val))]
	(write-to-buf codec buf val)
	[(.rewind ^ByteBuffer buf)]))

    (unbounded-writer? codec)
    (create-buf codec val)))

(defn frame-seq [reader buf-seq]
  (loop [result [], buf-seq buf-seq]
    (let [[x xs] (read-bytes reader buf-seq)]
      (if-not (reader? x)
	(recur (conj result x) xs)
	result))))

(defn- read-callback [codec callback]
  (reify
    Reader
    (read-bytes [this buf-seq]
      (let [[x bytes :as result] (read-bytes codec buf-seq)]
	(if (reader? x)
	  [this bytes]
	  (apply callback result))))
    UnboundedWriter
    (create-buf [_ val]
      (write-bytes codec val))))

(defn compose-readers [codec & readers]
  (let [readers (map
		  (fn [rd]
		    (if (satisfies? Reader rd)
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
      UnboundedWriter
      (create-buf [_ val]
	(let [header (body->header val)]
	  (concat
	    (write-bytes sig (body->header val))
	    (write-bytes (header->body header) val)))))))

;;;

(defn- repeated-reader [codec len vals]
  (reify
    Reader
    (read-bytes [_ buf-seq]
      (loop [buf-seq buf-seq, vals vals]
	(if (= (count vals) len)
	  [vals buf-seq]
	  (let [[v b] (read-bytes codec buf-seq)]
	    (if (reader? v)
	      [(repeated-reader codec len vals) b]
	      (recur b (conj vals v)))))))))

(defn wrap-prefix-repeated
  [prefix-codec codec]
  (assert (bounded-writer? prefix-codec))
  (let [codec* (compose-readers
		 prefix-codec
		 (fn [len b]
		   (read-bytes (repeated-reader codec len []) b)))
	sizeof-prefix (sizeof prefix-codec 0)]
    (if (bounded-writer? codec)
     (reify
       Reader
       (read-bytes [_ buf-seq]
	 (read-bytes codec* buf-seq))
       BoundedWriter
       (sizeof [_ vals]
	 (+ sizeof-prefix (apply + (map #(sizeof codec %) vals))))
       (write-to-buf [_ buf vals]
	 (write-to-buf prefix-codec buf (count vals))
	 (doseq [v vals]
	   (write-to-buf codec buf v))))
     (reify
       Reader
       (read-bytes [_ buf-seq]
	 (read-bytes codec* buf-seq))
       UnboundedWriter
       (create-buf [_ vals]
	 (concat
	   (write-bytes prefix-codec (count vals))
	   (apply concat (map #(write-bytes codec %) vals))))))))
