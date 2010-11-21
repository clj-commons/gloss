;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns gloss.core.frame
  (:use
    [clojure.walk]
    [gloss.core protocols]
    [gloss.data primitives bytes])
  (:require
    [clojure.zip :as z])
  (:import
    [java.nio
     ByteBuffer]))

;;;

(defn header [codec header->body body->header]
  (let [read-codec (compose-readers
		     codec
		     (fn [v b]
		       (let [body (header->body v)]
			 (read-bytes body b))))]
    (reify
      Reader
      (read-bytes [_ buf-seq]
	(read-bytes read-codec buf-seq))
      Writer
      (sizeof [_]
	nil)
      (write-bytes [_ buf val]
	(let [header (body->header val)]
	  (concat
	    (write-bytes codec buf (body->header val))
	    (write-bytes (header->body header) buf val)))))))

;;;

(declare read-prefixed-sequence)

(defn- insufficient-bytes? [codec buf-seq len vals]
  (when-let [size (sizeof codec)]
    (< (buf-seq-count buf-seq) (* size (- len (count vals))))))

(defn- prefixed-sequence-reader [codec reader len vals]
  (reify
    Reader
    (read-bytes [this buf-seq]
      (if (insufficient-bytes? codec buf-seq len vals)
	[false this buf-seq]
	(read-prefixed-sequence codec reader buf-seq len vals)))))

(defn- read-prefixed-sequence [codec reader buf-seq len vals]
  (loop [buf-seq buf-seq, vals vals, reader reader]
    (if (= (count vals) len)
      [true vals buf-seq]
      (let [[success x b] (read-bytes reader buf-seq)]
	(if success
	  (recur b (conj vals x) codec)
	  [false (prefixed-sequence-reader codec x len vals) b])))))

(defn wrap-prefixed-sequence
  [prefix-codec codec]
  (assert (sizeof prefix-codec))
  (let [read-codec (compose-readers
		     prefix-codec
		     (fn [len b]
		       (if (insufficient-bytes? codec b len nil)
			 [false (prefixed-sequence-reader codec codec len []) b]
			 (read-prefixed-sequence codec codec b len []))))
	sizeof-prefix (sizeof prefix-codec)]
    (reify
      Reader
      (read-bytes [_ b]
	(read-bytes read-codec b))
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

(defn prefix
  [codec to-integer from-integer]
  (let [read-codec (compose-readers
		     codec
		     (fn [x b]
		       [true (to-integer x) b]))]
    (reify
      Reader
      (read-bytes [_ b]
	(read-bytes read-codec b))
      Writer
      (sizeof [_]
	(sizeof codec))
      (write-bytes [_ buf v]
	(write-bytes codec buf (from-integer v))))))

;;;



(defn- sequence-reader
  [result readers]
  (reify Reader
    (read-bytes [_ b]
      (loop [b b, s readers, result result]
	(if (empty? s)
	  [true result b]
	  (let [[reader? x] (first s)]
	    (if-not reader?
	      (recur b (rest s) (conj result x))
	      (let [[success x b] (read-bytes x b)]
		(if success
		  (recur b (rest s) (conj result x))
		  [false
		   (compose-readers
		     x
		     (fn [v b]
		       (read-bytes (sequence-reader (conj result v) (rest s)) b)))
		   b])))))))))

(defn convert-sequence
  [frame]
  (let [finite-frame? (every? sizeof (filter reader? frame))
	s (map list (map reader? frame) frame)
	codecs (filter reader? frame)
	sizeof (when finite-frame? (apply + (map sizeof codecs)))
	reader (sequence-reader [] s)]
    (reify
      Reader
      (read-bytes [this b]
	(if (and sizeof (< (buf-seq-count b) sizeof))
	  [false this b]
	  (read-bytes reader b)))
      Writer
      (sizeof [_] sizeof)
      (write-bytes [_ buf vs]
	(if finite-frame?
	  (with-buffer [buf sizeof]
	    (doseq [[[_ x] v] (filter ffirst (map list s vs))]
	      (write-bytes x buf v)))
	  (apply concat
	    (map
	      (fn [[[_ x] v]] (write-bytes x buf v))
	      (filter ffirst (map list s vs)))))))))

(defn convert-map
  [frame]
  (let [ks (sort (keys frame))
	vs (map frame ks)
	codec (convert-sequence vs)
	read-codec (compose-readers 
		     codec
		     (fn [x b]
		       [true (zipmap ks x) b]))]
    (reify
      Reader
      (read-bytes [_ b]
	(read-bytes read-codec b))
      Writer
      (sizeof [_]
	(sizeof codec))
      (write-bytes [_ buf v]
	(write-bytes codec buf (map v ks))))))

(defn- compile-frame- [f]
  (cond
    (map? f) (convert-map (zipmap (keys f) (map compile-frame- (vals f))))
    (sequential? f) (convert-sequence (map compile-frame- f))
    :else f))

(defn compile-frame [f]
  (if (reader? f)
    f
    (->> f
     (postwalk-replace primitive-codecs)
     compile-frame-)))




