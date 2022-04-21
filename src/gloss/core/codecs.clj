;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^{:skip-wiki true}
  gloss.core.codecs
  (:require
    [gloss.core.formats :refer :all]
		[gloss.core.protocols :refer :all]
		[gloss.core.structure :refer :all]
		[gloss.data.bytes :refer :all ]
		[gloss.data.primitives :refer :all]
		[gloss.data.string :refer :all]))

;;;

(defn header [codec header->body body->header]
  (let [read-codec (compose-callback
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
	(let [header (body->header val)
	      body (header->body header)]
	  (if (and (sizeof codec) (sizeof body))
	    (with-buffer [buf (+ (sizeof codec) (sizeof body))]
	      (write-bytes codec buf header)
	      (write-bytes body buf val))
	    (concat
	      (write-bytes codec buf header)
	      (write-bytes body buf val))))))))

(defn prefix
  [codec to-integer from-integer]
  (let [read-codec (compose-callback
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

(defn constant-prefix
  [len]
  (reify
    Reader
    (read-bytes [_ b]
      [true len b])
    Writer
    (sizeof [_]
      len)
    (write-bytes [_ buf v]
      nil)))

(def identity-codec
  (reify
    Reader
    (read-bytes [_ b]
      [true (dup-bytes b) nil])
    Writer
    (sizeof [_]
      nil)
    (write-bytes [_ _ b]
      (-> b to-buf-seq dup-bytes))))

;;;

(declare read-prefixed-sequence)

(defn- insufficient-bytes? [codec buf-seq len vals]
  (when-let [size (sizeof codec)]
    (< (byte-count buf-seq) (* size (- len (count vals))))))

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
  (let [read-codec (compose-callback
		     (compile-frame prefix-codec)
		     (fn [len b]
		       (cond
			 (zero? len)
			 [true nil b]
		       
			 (insufficient-bytes? codec b len nil)
			 [false (prefixed-sequence-reader codec codec len []) b]

			 :else
			 (read-prefixed-sequence codec codec b len []))))]
    (reify
      Reader
      (read-bytes [_ b]
	(read-bytes read-codec b))
      Writer
      (sizeof [_]
	nil)
      (write-bytes [_ buf vs]
	(when-not (sequential? vs)
	  (throw (Exception. (str "Expected a sequence, but got " vs))))
	(let [cnt (count vs)]
	  (if (and (sizeof prefix-codec) (sizeof codec))
	    (with-buffer [buf (+ (sizeof prefix-codec) (* cnt (sizeof codec)))]
	      (write-bytes prefix-codec buf cnt)
	      (doseq [v vs]
		(write-bytes codec buf v)))
	    (concat
	      (write-bytes prefix-codec buf cnt)
	      (apply concat
		(map #(write-bytes codec buf %) vs)))))))))

;;;

(defn wrap-suffixed-codec
  [suffix codec]
  (if (nil? suffix)
    codec
    (let [suffix (-> suffix to-byte-buffer to-buf-seq)
	  suffix-length (byte-count suffix)]
      (reify
	Reader
	(read-bytes [this b]
	  (let [len (byte-count b)
		available (- len suffix-length)]
	    (if (<= 0 available)
	      (let [[success x remainder] (read-bytes codec (take-bytes (dup-bytes b) available))]
		(if success
		  [true x (concat-bytes
			    (drop-bytes remainder suffix-length)
			    (when-not (empty? remainder)
			      (drop-bytes b available)))]
		  [false this b]))
	      [false this b])))
	Writer
	(sizeof [_]
	  nil)
	(write-bytes [_ buf vs]
	  (concat
	    (write-bytes codec nil vs)
	    suffix))))))

;;;

(defn enum
  "Takes a list of enumerations, or a map of enumerations onto values, and returns
   a codec which associates each enumeration with a unique encoded value.

   (enum :byte :a :b :c)
   (enum :int32 {:a 100, :b 200, :c 300})"
  [primitive-type & map-or-seq]
  (assert (primitive-codecs primitive-type))
  (let [coerce #(if (char? %)
                  (long (int %))
                  (long %))
        n->v (if (and (= 1 (count map-or-seq)) (map? (first map-or-seq)))
	       (let [m (first map-or-seq)]
		 (zipmap
		   (map coerce (vals m))
		   (keys m)))
	       (zipmap
		 (map coerce (range (count map-or-seq)))
		 map-or-seq))
	v->n (zipmap (vals n->v) (keys n->v))
	codec (primitive-codecs primitive-type)]
    (reify
      Reader
      (read-bytes [this b]
	(let [[success x b] (read-bytes codec b)]
	  (if success
	    [true (n->v (coerce x)) b]
	    [false this b])))
      Writer
      (sizeof [_]
	(sizeof codec))
      (write-bytes [_ buf v]
	(if-let [n (v->n v)]
	  (write-bytes codec buf n)
	  (throw (Exception. (str "Expected one of " (keys v->n) ", but got " v))))))))

;;;

(defn ordered-map
  "Creates a codec which consumes and emits standard Clojure hash-maps, but
   ensures that the values are encoded in the specified order.  Useful for
   interop with C structs and network protocols."
  [& key-value-pairs]
  (assert (even? (count key-value-pairs)))
  (let [pairs (partition 2 key-value-pairs)
	ks (map first pairs)
	vs (map compile-frame (map second pairs))
	codec (convert-sequence vs)
	read-codec (compose-callback
		     codec
		     (fn [v b] [true (zipmap ks v) b]))]
    (reify
      Reader
      (read-bytes [_ b]
	(read-bytes read-codec b))
      Writer
      (sizeof [_]
	(sizeof codec))
      (write-bytes [_ buf v]
	(when-not (map? v)
	  (throw (Exception. (str "Expected a map, but got " v))))
	(write-bytes codec buf (map #(get v %) ks))))))
