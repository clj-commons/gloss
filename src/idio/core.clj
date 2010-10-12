;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns idio.core
  (:import
    [java.nio
     ByteBuffer]))

(defprotocol BufferSeq
  (buffer-seq [s])
  (concat-buffer [s b]))

(defn byte-seq [^ByteBuffer buf]
  (let [buf (.duplicate buf)]
    (lazy-seq
      (when (.hasRemaining buf)
	(cons (.get buf) (byte-seq buf))))))

(defn- byte-count [buf-seq]
  (apply + (map #(.remaining ^ByteBuffer %) buf-seq)))

(defn drop-bytes [bytes buf-seq]
  (loop [remaining bytes, s buf-seq]
    (let [buf ^ByteBuffer (first s)]
      (if (< remaining (.remaining buf))
	(cons (-> buf .duplicate (.position (+ remaining (.position buf))) .slice) (rest s))
	(recur (- remaining (.remaining buf)) (rest s))))))

(defn take-contiguous-bytes [bytes buf-seq]
  (let [first-buf ^ByteBuffer (first buf-seq)]
    (if (> (.remaining first-buf) bytes)
      (-> first-buf .duplicate (.limit (+ (.position first-buf) bytes)) .slice)
      (when (< bytes (byte-count buf-seq))
	(let [ary (byte-array bytes)]
	  (loop [offset 0, s buf-seq]
	    (if (>= offset bytes)
	      (ByteBuffer/wrap ary)
	      (let [buf ^ByteBuffer (first s)
		    take-bytes (min (.remaining buf) (- bytes offset))]
		(-> buf .duplicate (.get ary offset take-bytes))
		(recur (+ offset take-bytes) (rest s))))))))))

(defn take-bytes [bytes buf-seq]
  (let [first-buf ^ByteBuffer (first buf-seq)]
    (if (> (.remaining first-buf) bytes)
      [(-> first-buf .duplicate (.limit (+ (.position first-buf) bytes)) .slice)]
      (when (< bytes (byte-count buf-seq))
	(loop [remaining bytes, s buf-seq, target []]
	  (if (pos? remaining)
	    (let [buf ^ByteBuffer (first s)]
	      (if (>= remaining (.remaining buf))
		(recur (- remaining (.remaining buf)) (rest s) (conj target buf))
		(conj target (-> buf .duplicate (.limit (+ (.position buf) remaining)) .slice))))
	    target))))))

;;naive solution - make this better
(defn take-delimited-bytes [delimiters buf-seq strip-delimiters?]
  (let [delimiters (reverse (sort-by #(.remaining ^ByteBuffer %) delimiters))
	first-byte (fn this [s]
		     (let [buf ^ByteBuffer (first s)]
		       (.get buf (.position buf))))
	advance (fn this [s]
		  (let [buf ^ByteBuffer (first s)]
		    (.slice (.position buf (inc (.position buf))))
		    (if (.hasRemaining buf)
		      s
		      (rest s))))]
    (loop [s (map #(.duplicate ^ByteBuffer %) buf-seq)]
      (if (empty? s)
	[nil buf-seq]
	(if-let [delimiter (some
			     (fn [^ByteBuffer delimiter]
			       (and
				 (= (.get delimiter 0) (first-byte s))
				 (= delimiter (take-contiguous-bytes (.remaining delimiter) s))
				 delimiter))
			     delimiters)]
	  (let [location (- (byte-count buf-seq) (byte-count s))
		delimiter-count (.remaining ^ByteBuffer delimiter)]
	    [(take-bytes (+ location (if strip-delimiters? 0 delimiter-count)) buf-seq)
	     (drop-bytes (+ location delimiter-count) buf-seq)])
	  (recur (advance s)))))))
