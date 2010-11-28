;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns gloss.data.bytes.core
  (:import
    [java.nio
     Buffer
     ByteBuffer]))

;;;

(defn duplicate [^ByteBuffer buf]
  (-> buf .duplicate (.order (.order buf))))

(defn slice [^ByteBuffer buf]
  (-> buf .slice (.order (.order buf))))

(defn byte-count [buf-seq]
  (apply + (map #(.remaining ^Buffer %) buf-seq)))

(defn write-to-buf [buf-seq buf]
  (doseq [b buf-seq]
    (.put ^ByteBuffer buf ^ByteBuffer b)))

(defn rewind-bytes [buf-seq]
  (concat
    (map #(.rewind ^ByteBuffer %) (drop-last buf-seq))
    (let [last-buf ^ByteBuffer (last buf-seq)
	  last-pos (.position last-buf)]
      [(-> last-buf (.position 0) ^ByteBuffer (.limit last-pos) slice)])))

(defn dup-bytes [buf-seq]
  (map duplicate buf-seq))

(defn drop-bytes
  [n buf-seq]
  (cond
    (not (pos? n))
    buf-seq

    (>= n (byte-count buf-seq))
    nil

    :else
    (loop [remaining n, s buf-seq]
      (when-not (empty? s)
	(let [buf ^ByteBuffer (first s)]
	  (cond
	    (= remaining (.remaining buf))
	    (rest s)

	    (< remaining (.remaining buf))
	    (cons
	      (-> buf duplicate ^ByteBuffer (.position (+ remaining (.position buf))) slice)
	      (rest s))

	    :else
	    (recur (- remaining (.remaining buf)) (rest s))))))))

(defn take-bytes
  [n buf-seq]
  (cond
    (not (pos? n))
    nil

    (>= n (byte-count buf-seq))
    buf-seq

    :else
    (when-let [first-buf ^ByteBuffer (first buf-seq)]
      (if (> (.remaining first-buf) n)
	[(-> first-buf duplicate ^ByteBuffer (.limit (+ (.position first-buf) n)) slice)]
	(when (<= n (byte-count buf-seq))
	  (loop [remaining n, bytes buf-seq, accumulator []]
	    (if (pos? remaining)
	      (let [buf ^ByteBuffer (first bytes)]
		(if (>= remaining (.remaining buf))
		  (recur (- remaining (.remaining buf)) (rest bytes) (conj accumulator buf))
		  (conj accumulator (-> buf duplicate ^ByteBuffer (.limit (+ (.position buf) remaining)) slice))))
	      accumulator)))))))

(defn take-contiguous-bytes
  "Returns a single ByteBuffer which contains the first 'n' bytes of the buffer-sequence."
  [n buf-seq]
  (when-not (or (empty? buf-seq) (< (byte-count buf-seq) n))
    (let [buf-seq (dup-bytes buf-seq)
	  first-buf ^ByteBuffer (first buf-seq)]
      (if (> (.remaining first-buf) n)
	(-> first-buf duplicate ^ByteBuffer (.limit (+ (.position first-buf) n)) slice)
	(when (and (pos? n) (<= n (byte-count buf-seq)))
	  (let [ary (byte-array n)]
	    (loop [offset 0, bytes buf-seq]
	      (if (>= offset n)
		(ByteBuffer/wrap ary)
		(let [buf ^ByteBuffer (first bytes)
		      num-bytes (min (.remaining buf) (- n offset))]
		  (-> buf duplicate (.get ary offset num-bytes))
		  (recur (+ offset num-bytes) (rest bytes)))))))))))
