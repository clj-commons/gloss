;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns gloss.core
  (:import
    [java.nio
     ByteBuffer]))

(defprotocol BufferSeq
  (buffer-seq [s])
  (remainder-bytes [s])
  (concat-buffer [s b]))

(defn byte-buffer [s]
  (if (string? s)
    (ByteBuffer/wrap (.getBytes ^String s "utf-8"))
    (ByteBuffer/wrap (byte-array (map byte s)))))

(defn buf-seq-count [buf-seq]
  (apply + (map #(.remaining ^ByteBuffer %) buf-seq)))

(defn drop-bytes
  "Returns the buffer-sequence less the first 'n' bytes."
  [n buf-seq]
  (loop [remaining n, s buf-seq]
    (when-not (empty? s)
      (let [buf ^ByteBuffer (first s)]
	(if (< remaining (.remaining buf))
	  (cons (-> buf .duplicate (.position (+ remaining (.position buf))) .slice) (rest s))
	  (recur (- remaining (.remaining buf)) (rest s)))))))

(defn take-contiguous-bytes
  "Returns a single ByteBuffer which contains the first 'n' bytes of the buffer-sequence."
  [n buf-seq]
  (let [first-buf ^ByteBuffer (first buf-seq)]
    (if (> (.remaining first-buf) n)
      (-> first-buf .duplicate (.limit (+ (.position first-buf) n)) .slice)
      (when (<= n (buf-seq-count buf-seq))
	(let [ary (byte-array bytes)]
	  (loop [offset 0, s buf-seq]
	    (if (>= offset n)
	      (ByteBuffer/wrap ary)
	      (let [buf ^ByteBuffer (first s)
		    take-bytes (min (.remaining buf) (- n offset))]
		(-> buf .duplicate (.get ary offset take-bytes))
		(recur (+ offset take-bytes) (rest s))))))))))

(defn take-bytes
  "Returns a buffer-sequence containing the first 'n' bytes of the given buffer-sequence."
  [n buf-seq]
  (let [first-buf ^ByteBuffer (first buf-seq)]
    (if (> (.remaining first-buf) n)
      [(-> first-buf .duplicate (.limit (+ (.position first-buf) n)) .slice)]
      (when (<= n (buf-seq-count buf-seq))
	(loop [remaining n, s buf-seq, accumulator []]
	  (if (pos? remaining)
	    (let [buf ^ByteBuffer (first s)]
	      (if (>= remaining (.remaining buf))
		(recur (- remaining (.remaining buf)) (rest s) (conj accumulator buf))
		(conj accumulator (-> buf .duplicate (.limit (+ (.position buf) remaining)) .slice))))
	    accumulator))))))

;;naive solution - this assumes small and dissimilar delimiters
(defn take-delimited-bytes
  "Returns the buffer-sequence as two buffer-sequences, split around the first found
   instance of a delimiter."
  ([delimiters buf-seq]
     (take-delimited-bytes delimiters seq true))
  ([delimiters buf-seq strip-delimiters?]
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
				    (or
				      (= 1 (.remaining delimiter))
				      (= delimiter (take-contiguous-bytes (.remaining delimiter) s)))
				    delimiter))
				delimiters)]
	     (let [location (- (buf-seq-count buf-seq) (byte-count s))
		   delimiter-count (.remaining ^ByteBuffer delimiter)]
	       [(take-bytes (+ location (if strip-delimiters? 0 delimiter-count)) buf-seq)
		(drop-bytes (+ location delimiter-count) buf-seq)])
	     (recur (advance s))))))))
