;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns gloss.consumer
  (:import
    [java.nio
     ByteBuffer]))

(defprotocol ByteConsumer
  (feed- [this buf-seq]))

(def byte-array-class (class (byte-array [])))

(defn to-buf-seq [x]
  (when x
    (cond
      (and (sequential? x) (or (empty? x) (instance? ByteBuffer (first x)))) x
      (= (class x) byte-array-class) [(ByteBuffer/wrap x)]
      (instance? ByteBuffer x) [x]
      :else (throw (Exception. (str "Cannot convert to buffer-seq: " x))))))

(defn feed [consumer bytes]
  (feed- consumer (to-buf-seq bytes)))

(defn consumer? [x]
  (satisfies? ByteConsumer x))

(defn consumer-seq [consumer buf-seq]
  (loop [result [], buf-seq buf-seq]
    (let [[x xs] (feed consumer buf-seq)]
      (if-not (consumer? x)
	(recur (conj result x) xs)
	result))))
