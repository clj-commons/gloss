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
    [gloss.data primitives])
  (:require
    [clojure.zip :as z])
  (:import
    [java.nio
     ByteBuffer]))

(declare compile-frame)

;;;

(defn sequential-zip [s]
  (z/zipper
    #(or (sequential? %) (map? %))
    seq
    (fn [node children] (when children (with-meta children (meta node))))
    s))

(defn- backtrack [& zs]
  (loop [zs zs]
    (let [z (first zs)]
      (cond
	(or (z/end? z) (not (z/path z))) zs
	(z/right z) (map z/right zs)
	:else (recur (map z/up zs))))))

(defn match-structure [frame root]
  (loop [frame (sequential-zip frame), s (sequential-zip root)]
    (if (z/end? frame)
      (z/root frame)
      (if-not (z/down frame)
	(let [frame (z/edit frame #(with-meta (list % (z/node s)) {::match true}))
	      [frame s] (backtrack frame s)]
	  (if-not (z/path frame)
	    (z/root frame)
	    (recur frame s)))
	(recur (z/next frame) (z/next s))))))

(defn matches [frame root]
  (let [s (match-structure frame root)]
    ((fn walk [s]
       (cond
	 (-> s meta ::match) [s]
	 (sequential? s) (apply concat (map walk s))
	 :else nil))
     s)))

(defn find-and-replace [pred editor root]
  (loop [s (sequential-zip root)]
    (if (z/end? s)
      (z/root s)
      (let [n (z/node s)]
	(if (pred n)
	  (recur (-> s (z/edit editor) z/next))
	  (recur (z/next s)))))))

;;;

(defn gather-values [frame s]
  (filter #(writer? (first %)) (matches frame s)))

(defn- scatter-values- [zip buf-seq]
  (loop [frame zip, bytes buf-seq]
    (if (z/end? frame)
      [(z/root frame) bytes]
      (let [f (z/node frame)]
	(if (reader? f)
	  (let [[val bytes] (read-bytes f bytes)]
	    (if (reader? val)
	      [(reify Reader
		 (read-bytes [_ buf-seq]
		   (scatter-values- frame bytes)))
	       bytes]
	      (recur (-> frame (z/edit (constantly val)) z/next) bytes)))
	  (recur (z/next frame) bytes))))))

(defn scatter-values [frame buf-seq]
  (if-not (or (map? frame) (sequential? frame))
    (read-bytes frame buf-seq)
    (scatter-values- (sequential-zip frame) buf-seq)))

;;;

(defn compile-primitives [frame]
  (postwalk-replace primitive-codecs frame))

;;;

(defn map-codec [m]
  (let [k (sort (keys m))
	frame (interleave k (map #(% m) k))
	codec (read-comp
		(compile-frame frame)
		(fn [[v b]] [(apply hash-map v) b]))]
    (reify
      Reader
      (read-bytes [this buf-seq]
	(read-bytes codec buf-seq))
      UnboundedWriter
      (create-buf [this val]
	(write-bytes codec (interleave k (map #(% val) k)))))))

(defn compile-maps [frame]
  (prewalk
    #(if (map? %)
       (map-codec %)
       %)
    frame))

;;;

(defn flatten-frame [frame]
  (flatten (postwalk #(if (map? %) (seq %) %) frame)))

(defn contiguous-bounded-writers? [frame]
  (let [w (->> frame
	    flatten-frame
	    (filter writer?))]
    (and (pos? (count w)) (every? bounded-writer? w))))

(defn bounded-writers-codec [frame]
  (let [writers (filter writer? (flatten-frame frame))
	len (apply + (map size writers))]
    (reify
      Reader
      (read-bytes [this buf-seq]
	(scatter-values frame buf-seq))
      UnboundedWriter
      (create-buf [this val]
	(let [buf (ByteBuffer/allocate len)]
	  [(.rewind ^ByteBuffer
	     (reduce
	       (fn [b [w v]] (write-to-buf w b v))
	       buf
	       (gather-values frame val)))])))))

(defn compile-contiguous-writers [frame]
  (prewalk
    #(if (contiguous-bounded-writers? %)
       (bounded-writers-codec %)
       %)
    frame))

;;;

(defn compile-frame [f]
  (let [frame (->> f
		compile-primitives
		compile-maps
		compile-contiguous-writers)]
    (reify
      Reader
      (read-bytes [this buf-seq]
	(scatter-values frame buf-seq))
      UnboundedWriter
      (create-buf [this val]
	(mapcat
	  (fn [[w v]] (write-bytes w v))
	  (gather-values frame val))))))
