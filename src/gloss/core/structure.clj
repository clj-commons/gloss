;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^{:skip-wiki true}
  gloss.core.structure
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
    (read-bytes [_ b bounded?]
      (loop [b b, s readers, result result]
	(if (empty? s)
	  [true result b]
	  (let [[reader? x] (first s)]
	    (if-not reader?
	      (recur b (rest s) (conj result x))
	      (let [[success x b] (read-bytes x b false)] ;;TODO: it's not always false
		(if success
		  (recur b (rest s) (conj result x))
		  [false
		   (compose-callback
		     x
		     (fn [v b _]
		       (read-bytes
			 (sequence-reader (conj result v) (rest s))
			 b
			 false))) ;;TODO: sometimes not false?
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
      (read-bytes [this b bounded?]
	(if (and (not bounded?) sizeof (< (byte-count b) sizeof))
	  [false this b]
	  (read-bytes reader b bounded?)))
      Writer
      (sizeof [_] sizeof)
      (write-bytes [_ buf vs]
	(when-not (sequential? vs)
	  (throw (Exception. (str "Expected a sequence, but got " vs))))
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
	read-codec (compose-callback 
		     codec
		     (fn [x b _]
		       [true (zipmap ks x) b]))]
    (reify
      Reader
      (read-bytes [_ b bounded?]
	(read-bytes read-codec b bounded?))
      Writer
      (sizeof [_]
	(sizeof codec))
      (write-bytes [_ buf v]
	(when-not (map? v)
	  (throw (Exception. (str "Expected a map, but got " v))))
	(write-bytes codec buf (map v ks))))))

(defn- compile-frame- [f]
  (cond
    (map? f) (convert-map (zipmap (keys f) (map compile-frame- (vals f))))
    (sequential? f) (convert-sequence (map compile-frame- f))
    :else f))

(defn compile-frame
  "Takes a frame, and returns a codec.  This function is idempotent; passing in a codec
   is a safe operation.

   Functions that transform the values after they are decoded and before they are encoded
   can be specified, which allows the frame to only be an intermediate representation of
   the final Clojure data structure."
  ([frame]
     (if (reader? frame)
       frame
       (->> frame
	 (postwalk-replace primitive-codecs)
	 compile-frame-)))
  ([frame pre-encoder post-decoder]
     (let [codec (compile-frame frame)
	   read-codec (compose-callback
			codec
			(fn [x b _]
			  [true (post-decoder x) b]))]
       (reify
	 Reader
	 (read-bytes [_ b bounded?]
	   (read-bytes read-codec b bounded?))
	 Writer
	 (sizeof [_]
	   (sizeof codec))
	 (write-bytes [_ buf v]
	   (write-bytes codec buf (pre-encoder v)))))))




