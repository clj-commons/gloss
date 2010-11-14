;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns gloss.data.bytes
  (:use
    gloss.core.protocols)
  (:import
    [java.nio
     Buffer
     ByteBuffer]))

(defprotocol BufferSeqWrapper
  (buffer-seq [s])
  (remainder-bytes [s])
  (concat-bytes [s bytes]))

(defn byte-buffer [s]
  (if (string? s)
    (ByteBuffer/wrap (.getBytes ^String s "utf-8"))
    (ByteBuffer/wrap (byte-array (map byte s)))))

(defn write-buf-seq [buf-seq buf]
  (doseq [b buf-seq]
    (.put ^ByteBuffer buf b)))

(defn rewind-buf-seq [buf-seq]
  (concat
    (map #(.rewind ^Buffer %) (drop-last buf-seq))
    (let [last-buf (last buf-seq)
	  last-pos (.position last-buf)]
      [(-> last-buf (.position 0) (.limit last-pos) .slice)])))

(defn buf-seq-count [buf-seq]
  (apply + (map #(.remaining ^Buffer %) buf-seq)))

(defn dup-buf-seq [buf-seq]
  (map #(.duplicate ^Buffer %) buf-seq))

(defn drop-from-bufs
  [n buf-seq]
  (loop [remaining n, s buf-seq]
    (when-not (empty? s)
      (let [buf ^Buffer (first s)]
	(if (< remaining (.remaining buf))
	  (cons (-> buf .duplicate (.position (+ remaining (.position buf))) .slice) (rest s))
	  (recur (- remaining (.remaining buf)) (rest s)))))))

(defn take-from-bufs
  [n buf-seq]
  (let [first-buf ^Buffer (first buf-seq)]
    (when first-buf
      (if (> (.remaining first-buf) n)
	[(-> first-buf .duplicate (.limit (+ (.position first-buf) n)) .slice)]
	(when (<= n (buf-seq-count buf-seq))
	  (loop [remaining n, s buf-seq, accumulator []]
	    (if (pos? remaining)
	      (let [buf ^Buffer (first s)]
		(if (>= remaining (.remaining buf))
		  (recur (- remaining (.remaining buf)) (rest s) (conj accumulator buf))
		  (conj accumulator (-> buf .duplicate (.limit (+ (.position buf) remaining)) .slice))))
	      accumulator)))))))

(defn drop-bytes
  "Returns the buffer-sequence less the first 'n' bytes."
  [n bytes]
  (let [buf-seq (to-buf-seq bytes)]
    (drop-from-bufs n buf-seq)))

(defn take-contiguous-bytes
  "Returns a single ByteBuffer which contains the first 'n' bytes of the buffer-sequence."
  [n bytes]
  (let [buf-seq (to-buf-seq bytes)
	first-buf ^ByteBuffer (first buf-seq)]
    (if (> (.remaining first-buf) n)
      (-> first-buf .duplicate (.limit (+ (.position first-buf) n)) .slice)
      (when (<= n (buf-seq-count buf-seq))
	(let [ary (byte-array n)]
	  (loop [offset 0, s buf-seq]
	    (if (>= offset n)
	      (ByteBuffer/wrap ary)
	      (let [buf ^ByteBuffer (first s)
		    take-bytes (min (.remaining buf) (- n offset))]
		(-> buf .duplicate (.get ary offset take-bytes))
		(recur (+ offset take-bytes) (rest s))))))))))

(defn take-bytes
  "Returns a buffer-sequence containing the first 'n' bytes of the given buffer-sequence."
  [n bytes]
  (let [buf-seq (to-buf-seq bytes)]
    (take-from-bufs n buf-seq)))

;;naive solution - this assumes small and dissimilar delimiters
(defn take-delimited-bytes
  "Returns the buffer-sequence as two buffer-sequences, split around the first found
   instance of a delimiter."
  ([bytes delimiters]
     (take-delimited-bytes delimiters bytes true))
  ([bytes delimiters strip-delimiters?]
     (let [buf-seq (to-buf-seq bytes)
	   delimiters (reverse (sort-by #(.remaining ^ByteBuffer %) delimiters))
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
	     (let [location (- (buf-seq-count buf-seq) (buf-seq-count s))
		   delimiter-count (.remaining ^ByteBuffer delimiter)]
	       [(take-bytes (+ location (if strip-delimiters? 0 delimiter-count)) buf-seq)
		(drop-bytes (+ location delimiter-count) buf-seq)])
	     (recur (advance s))))))))

(defn finite-byte-handler
  [bytes len]
  (reify
    ByteConsumer
    (feed [this b]
      (if (< (buf-seq-count b) len)
	[this bytes]
	[(take-bytes len b) (drop-bytes len b)]))
    ByteEmitter
    (emit [this buf-seq]
      buf-seq)))

(defn delimited-byte-handler
  [bytes delimiters strip-delimiters?]
  (reify
    ByteConsumer
    (feed [this b]
      (let [[x xs] (take-delimited-bytes bytes delimiters strip-delimiters?)]
	(if x
	  [(concat bytes x) xs]
	  [(delimited-byte-handler xs delimiters strip-delimiters?)
	   nil])))
    ByteEmitter
    (emit [this buf-seq]
      (concat buf-seq [(first delimiters)]))))

 
