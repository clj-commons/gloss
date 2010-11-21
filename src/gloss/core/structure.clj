;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns gloss.core.structure
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
	(if (and sizeof (< (byte-count b) sizeof))
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




