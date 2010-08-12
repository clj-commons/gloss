;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns idio
  (:use
    [clojure.walk]
    [clojure.contrib.def])
  (:import [java.nio ByteOrder ByteBuffer]))

(defn- to-byte [b]
  (byte
    (if (instance? Boolean b)
      (if b 1 0)
      b)))

(defvar- put-fns
  {:float #(.putFloat ^ByteBuffer %1 %2)
   :double #(.putDouble ^ByteBuffer %1 %2)
   :int #(.putInt ^ByteBuffer %1 %2)
   :long #(.putLong ^ByteBuffer %1 %2)
   :short #(.putShort ^ByteBuffer %1 %2)
   :byte #(.put ^ByteBuffer %1 (to-byte %2))
   :char #(.putChar ^ByteBuffer %1 %2)})

(defvar- get-fns
  {:float #(.getFloat ^ByteBuffer %)
   :double #(.getDouble ^ByteBuffer %)
   :int #(.getInt ^ByteBuffer %)
   :long #(.getLong ^ByteBuffer %)
   :short #(.getShort ^ByteBuffer %)
   :byte #(.get ^ByteBuffer %)
   :char #(.getChar ^ByteBuffer %)})

(defprotocol Signature
  (num-bytes [s])
  (flattened [s])
  (signature [s])
  (get-element [s b]))

(defn- signature? [s]
  (= (->> s meta :type) ::signature))

(defvar- type?
  #{:float :double :byte :short :int :long :char})

(defn- create-signature [s]
  (if (signature? s)
    s
    (let [wrapped (if (sequential? s) s [s])
	  flattened (flatten (postwalk #(if (map? %) (vals %) %) wrapped))
	  num-bytes (reduce +
		      (map
			{:float 4
			 :double 8
			 :int 4
			 :long 8
			 :short 2
			 :byte 1}
			flattened))]
      ^{:type ::signature}
      (reify Signature
	(num-bytes [_] num-bytes)
	(flattened [_] flattened)
	(signature [_] s)
	(get-element [_ buf]
	  (postwalk
	    #(if (type? %)
	       ((get-fns %) buf)
	       %)
	    s))))))

;;;

(defn to-bytes
  "Fills a ByteBuffer with a contiguous mixed datatype defined
   by the signature."
  [s sig]
  (let [sig (create-signature sig)
	buffer-size (* (/ (count s) (count (flattened sig))) (num-bytes sig))
	buf (ByteBuffer/allocateDirect buffer-size)
	s (remove keyword? (flatten s))]
    (.order buf (ByteOrder/nativeOrder))
    (doseq [[typ val] (map list (cycle (flattened sig)) s)]
      ((put-fns typ) buf val))
    (.rewind ^ByteBuffer buf)
    buf))

(defn- element-from-buffer [^ByteBuffer buf sig idx]
  (let [buf ^ByteBuffer (.asReadOnlyBuffer buf)]
    (.order buf (ByteOrder/nativeOrder))
    (.position buf (* (num-bytes sig) idx))
    (get-element sig buf)))

(defn from-bytes
  "Pulls out mixed datatypes from a ByteBuffer, per the signature."
  [^ByteBuffer buf sig]
  (let [sig (create-signature sig)
	cnt (/ (.capacity buf) (num-bytes sig))]
    ^{:type ::from-buffer}
    (reify
      clojure.lang.Indexed
      clojure.lang.IPersistentCollection
      clojure.lang.Sequential
      clojure.lang.Seqable
      (nth [_ i] (element-from-buffer buf sig i))
      (count [_] cnt)
      (cons [x this] (cons x (seq this)))
      (empty [_] (= 0 cnt))
      (equiv [this o] (= (seq this) o))
      (seq [this] (map #(nth this %) (range cnt))))))
