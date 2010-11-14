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

(declare frame->emitter)
(declare frame->consumer)

(defn sequential-zip [s]
  (z/zipper
    #(or (sequential? %) (map? %))
    seq
    (fn [node children] (when children (with-meta children (meta node))))
    s))

(defn gather-values [frame val]
  (let [frame (sequential-zip frame)
	val (sequential-zip val)]
    (loop [frame frame, val val, matches []]
      (if (z/end? frame)
	matches
	(let [f (z/node frame)]
	  (if-not (or (writer? f) (emitter? f))
	    (recur (z/next frame) (z/next val) matches)
	    (recur
	      (-> frame z/remove z/next)
	      (-> val z/remove z/next)
	      (conj matches [f (z/node val)]))))))))

(defn- scatter-values- [zip buf-seq]
  (loop [frame zip, bytes buf-seq]
    (if (z/end? frame)
      (z/root frame)
      (let [f (z/node frame)]
       (if (consumer? f)
	 (let [[val bytes] (feed f bytes)]
	   (if (consumer? val)
	     [(reify ByteConsumer
		(feed [_ buf-seq]
		  (scatter-values- frame bytes))) bytes]
	     (recur (-> frame (z/edit (constantly val)) z/next) bytes)))
	 (recur (z/next frame) bytes))))))

(defn scatter-values [frame buf-seq]
  (scatter-values- (sequential-zip frame) buf-seq))

;;;

(defn convert-primitives [frame]
  (postwalk-replace primitive-handlers frame))

;;;

(defn ordered-tuples? [x]
  (and (map? x) (every? keyword? (map first (partition 2 x)))))

(defn ordered-tuples-wrapper [ordered-tuples]
  (let [emitter (frame->emitter ordered-tuples)
	consumer (consume-and-continue (frame->consumer ordered-tuples) #(apply hash-map %))
	names (map first (partition 2 ordered-tuples))]
    (reify
      ByteEmitter
      (emit [this val]
	(emit emitter (interleave names (map names val))))
      ByteConsumer
      (feed [this buf-seq]
	(feed consumer buf-seq)))))

(defn convert-ordered-tuples [frame]
  (postwalk
    #(if (ordered-tuples? %)
       (ordered-tuples-wrapper %)
       %)
    frame))

;;;

(defn flatten-frame [frame]
  (flatten (postwalk #(if (map? %) (seq %) %) frame)))

(defn contiguous-writers? [frame]
  (->> frame
    flatten-frame
    (filter #(or (writer? %) (emitter? %)))
    (every? writer?)))

(defn convert-contiguous-writers [frame]
  (let [writers (filter writer? (flatten-frame frame))
	len (apply + (map size writers))]
    (reify
      ByteEmitter
      (emit [this val]
	(let [buf (ByteBuffer/allocate len)]
	  (.rewind ^ByteBuffer
	    (reduce
	      (fn [b [w v]] (write w buf v))
	      buf
	      (gather-values frame val)))))
      ByteConsumer
      (feed [this buf-seq]
	(scatter-values frame buf-seq)))))

;;;

(defn preprocess-frame [frame]
  (->> frame
    convert-primitives
    convert-ordered-tuples
    convert-contiguous-writers))

