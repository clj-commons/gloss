;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns gloss.core.codecs
  (:use
    [gloss.data bytes string primitives]
    [gloss.core protocols]))

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
	(let [header (body->header val)]
	  (concat
	    (write-bytes codec buf (body->header val))
	    (write-bytes (header->body header) buf val)))))))

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
  (assert (sizeof prefix-codec))
  (let [read-codec (compose-callback
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

;;;

(defn enum [& map-or-seq]
  (let [n->v (if (and (= 1 (count map-or-seq)) (map? (first map-or-seq)))
	       (let [m (first map-or-seq)]
		 (zipmap
		   (map short (vals m))
		   (keys m)))
	       (zipmap
		 (map short (range (count map-or-seq)))
		 map-or-seq))
	v->n (zipmap (vals n->v) (keys n->v))
	codec (:int16 primitive-codecs)]
    (reify
      Reader
      (read-bytes [this b]
	(let [[success x b] (read-bytes codec b)]
	  (if success
	    [true (n->v (short x)) b]
	    [false this b])))
      Writer
      (sizeof [_]
	(sizeof codec))
      (write-bytes [_ buf v]
	(write-bytes codec buf (v->n v))))))
