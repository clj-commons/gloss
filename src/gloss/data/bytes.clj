;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns gloss.data.bytes
  (:use
    [gloss.core formats protocols])
  (:import
    [java.nio
     Buffer
     ByteBuffer]))

(defn byte-buffer [s]
  (if (string? s)
    (ByteBuffer/wrap (.getBytes ^String s "utf-8"))
    (ByteBuffer/wrap (byte-array (map byte s)))))

(defn write-buf-seq [buf-seq buf]
  (doseq [b buf-seq]
    (.put ^ByteBuffer buf ^ByteBuffer b)))

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
  (when-not (empty? bytes)
    (let [buf-seq (to-buf-seq bytes)
	  first-buf ^ByteBuffer (first buf-seq)]
      (if (> (.remaining first-buf) n)
	(-> first-buf .duplicate (.limit (+ (.position first-buf) n)) .slice)
	(when (and (pos? n) (<= n (buf-seq-count buf-seq)))
	  (let [ary (byte-array n)]
	    (loop [offset 0, s buf-seq]
	      (if (>= offset n)
		(ByteBuffer/wrap ary)
		(let [buf ^ByteBuffer (first s)
		      take-bytes (min (.remaining buf) (- n offset))]
		  (-> buf .duplicate (.get ary offset take-bytes))
		  (recur (+ offset take-bytes) (rest s)))))))))))

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
       (loop [s (dup-buf-seq buf-seq)]
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
				(dup-buf-seq delimiters))]
	     (let [location (- (buf-seq-count buf-seq) (buf-seq-count s))
		   delimiter-count (-> ^ByteBuffer delimiter .rewind .remaining)]
	       [(take-bytes (+ location (if strip-delimiters? 0 delimiter-count)) buf-seq)
		(drop-bytes (+ location delimiter-count) buf-seq)])
	     (recur (advance s))))))))

;;;

(defn take-all [codec]
  (fn [byte-seq remainder]
    (loop [byte-seq byte-seq, s []]
      (if (empty? byte-seq)
	[s remainder]
	(let [[v b] (read-bytes codec byte-seq)]
	  (when (reader? v)
	    (throw (Exception. "Cannot evenly divide byte-sequence")))
	  (recur b (conj s v)))))))

(defn finite-byte-codec
  [len]
  (reify
    Reader
    (read-bytes [this b]
      (if (< (buf-seq-count b) len)
	[this b]
	[(take-bytes len b) (drop-bytes len b)]))
    BoundedWriter
    (sizeof [_ _]
      len)
    (write-to-buf [this buf buf-seq]
      (write-buf-seq buf-seq buf))))

(defn wrap-finite-codec
  [codec len]
  (let [finite-codec (compose-readers
		       (finite-byte-codec len)
		       (take-all codec))]
    (if (bounded-writer? codec)
      (reify
	Reader
	(read-bytes [_ b]
	  (read-bytes finite-codec b))
	BoundedWriter
	(sizeof [_ _]
	  len)
	(write-to-buf [_ buf vals]
	  (doseq [v vals]
	    (write-to-buf finite-codec buf v))))
      (reify
	Reader
	(read-bytes [this b]
	  (read-bytes finite-codec b))
	BoundedWriter
	(sizeof [_ _]
	  len)
	(write-to-buf [_ buf buf-seq]
	  (write-buf-seq buf-seq buf))))))

;;;

(defn delimited-byte-codec
  ([delimiters strip-delimiters]
     (delimited-byte-codec nil delimiters strip-delimiters))
  ([processed-bufs delimiters strip-delimiters?]
     (let [delimiters (map to-byte-buffer delimiters)
	   sizeof-delimiter (.remaining ^ByteBuffer (first delimiters))
	   max-trailing-delimiter (dec (apply max (map #(.remaining ^ByteBuffer %) delimiters)))
	   prefix (when-let [b (take-contiguous-bytes max-trailing-delimiter processed-bufs)]
		    [b])]
       (reify
	 Reader
	 (read-bytes [this b]
	   (let [[x xs] (take-delimited-bytes (concat prefix b) delimiters strip-delimiters?)]
	     (if x
	       [x xs]
	       [(delimited-byte-codec b delimiters strip-delimiters?)
		nil])))
	 BoundedWriter
	 (sizeof [_ buf-seq]
	   (+
	     (buf-seq-count buf-seq)
	     (if strip-delimiters? sizeof-delimiter 0)))
	 (write-to-buf [this buf buf-seq]
	   (if strip-delimiters?
	     (write-buf-seq (concat buf-seq (take 1 delimiters)) buf)
	     (write-buf-seq buf-seq buf)))))))

(defn delimited-block
  [codec delimiters]
  (let [delimiters (map to-byte-buffer delimiters)
	sizeof-delimiter (.remaining ^ByteBuffer (first delimiters))
	delimited-codec (compose-readers (delimited-byte-codec delimiters true)
			  (fn [bytes remainder]
			    (let [[v remainder*] (read-bytes codec bytes)]
			      (when-not (empty? remainder*)
				(throw (Exception. "All bytes inside delimited block must be consumed")))
			      [v remainder])))]
    (if (bounded-writer? codec)
      (reify
	Reader
	(read-bytes [_ buf-seq]
	  (read-bytes delimited-codec buf-seq))
	BoundedWriter
	(sizeof [_ val]
	  (+ (sizeof codec val) sizeof-delimiter))
	(write-to-buf [_ buf val]
	  (write-to-buf codec buf val)
	  (.put ^ByteBuffer buf ^ByteBuffer (first delimiters))))
      (reify
	Reader
	(read-bytes [_ buf-seq]
	  (read-bytes delimited-codec buf-seq))
	UnboundedWriter
	(create-buf [_ val]
	  (write-bytes delimited-codec
	    (write-bytes codec val)))))))

(defn wrap-delimited-sequence
  [codec delimiters]
  (let [delimiters (map to-byte-buffer delimiters)
	sizeof-delimiter (.remaining ^ByteBuffer (first delimiters))
	delimited-codec (compose-readers (delimited-byte-codec delimiters true) (take-all codec))]
    (if (bounded-writer? codec)
      (reify
	Reader
	(read-bytes [_ buf-seq]
	  (read-bytes delimited-codec buf-seq))
	BoundedWriter
	(sizeof [_ vals]
	  (+ (apply + (map #(sizeof codec %) vals)) sizeof-delimiter))
	(write-to-buf [_ buf vals]
	  (doseq [v vals]
	    (write-to-buf codec buf v))
	  (.put ^ByteBuffer buf ^ByteBuffer (first delimiters))))
      (reify
	Reader
	(read-bytes [_ buf-seq]
	  (read-bytes delimited-codec buf-seq))
	UnboundedWriter
	(create-buf [_ vals]
	  (write-bytes delimited-codec
	    (apply concat
	      (map #(write-bytes codec %) vals))))))))

 
