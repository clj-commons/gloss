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
     Buffer
     ByteBuffer]))

(defprotocol BufferSeq
  (buffer-seq [s])
  (remainder-bytes [s])
  (concat-bytes [s bytes]))

(defn byte-buffer [s]
  (if (string? s)
    (ByteBuffer/wrap (.getBytes ^String s "utf-8"))
    (ByteBuffer/wrap (byte-array (map byte s)))))

(defn buffer-seq-count [buf-seq]
  (apply + (map #(.remaining ^ByteBuffer %) buf-seq)))

(defn dup-buffer-seq [buf-seq]
  (map #(.duplicate ^ByteBuffer %) buf-seq))

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
	(when (<= n (buffer-seq-count buf-seq))
	  (loop [remaining n, s buf-seq, accumulator []]
	    (if (pos? remaining)
	      (let [buf ^Buffer (first s)]
		(if (>= remaining (.remaining buf))
		  (recur (- remaining (.remaining buf)) (rest s) (conj accumulator buf))
		  (conj accumulator (-> buf .duplicate (.limit (+ (.position buf) remaining)) .slice))))
	      accumulator)))))))

(def byte-array-class (class (byte-array [])))

(defn to-buffer-seq [x]
  (when x
    (cond
      (and (sequential? x) (or (empty? x) (instance? ByteBuffer (first x)))) x
      (= (class x) byte-array-class) [(ByteBuffer/wrap x)]
      (instance? ByteBuffer x) [x]
      (satisfies? BufferSeq x) (buffer-seq x)
      :else (throw (Exception. (str "Cannot convert to buffer-seq: " x))))))

(defn drop-bytes
  "Returns the buffer-sequence less the first 'n' bytes."
  [n bytes]
  (let [buf-seq (to-buffer-seq bytes)]
    (drop-from-bufs n buf-seq)))

(defn take-contiguous-bytes
  "Returns a single ByteBuffer which contains the first 'n' bytes of the buffer-sequence."
  [n bytes]
  (let [buf-seq (to-buffer-seq bytes)
	first-buf ^ByteBuffer (first buf-seq)]
    (if (> (.remaining first-buf) n)
      (-> first-buf .duplicate (.limit (+ (.position first-buf) n)) .slice)
      (when (<= n (buffer-seq-count buf-seq))
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
  (let [buf-seq (to-buffer-seq bytes)]
    (take-from-bufs n buf-seq)))

;;naive solution - this assumes small and dissimilar delimiters
(defn take-delimited-bytes
  "Returns the buffer-sequence as two buffer-sequences, split around the first found
   instance of a delimiter."
  ([bytes delimiters]
     (take-delimited-bytes delimiters bytes true))
  ([bytes delimiters strip-delimiters?]
     (let [buf-seq (to-buffer-seq bytes)
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
	     (let [location (- (buffer-seq-count buf-seq) (buffer-seq-count s))
		   delimiter-count (.remaining ^ByteBuffer delimiter)]
	       [(take-bytes (+ location (if strip-delimiters? 0 delimiter-count)) buf-seq)
		(drop-bytes (+ location delimiter-count) buf-seq)])
	     (recur (advance s))))))))

(declare wrap-delimited-bytes)

(defn- create-delimited-seq [first rest remainder delimiters strip-delimiters?]
  (let [max-delimiter-length (apply max (map #(.remaining ^ByteBuffer %) delimiters))]
    (reify
      BufferSeq
      (buffer-seq [_] (concat remainder (when first [first]) rest))
      (remainder-bytes [_] remainder)
      (concat-bytes [_ bytes]
	(let [buf-seq (to-buffer-seq bytes)]
	  (if first
	    (create-delimited-seq first (concat rest buf-seq) remainder delimiters strip-delimiters?)
	    (let [remainder-overlap (min max-delimiter-length (buffer-seq-count remainder))
		  buf-seq (concat (take-bytes remainder-overlap remainder) buf-seq)
		  remainder (drop-bytes remainder-overlap remainder)
		  [x* xs*] (take-delimited-bytes buf-seq delimiters strip-delimiters?)
		  x* (when x* (concat remainder x*))
		  remainder* (when-not x* (concat remainder xs*))
		  xs* (when x* xs*)]
	      (create-delimited-seq x* xs* remainder* delimiters strip-delimiters?)))))
      clojure.lang.ISeq
      (seq [this]
	(when-let [f (first this)]
	  (cons f (seq (next this)))))
      (first [_] first)
      (next [_]
	(when (pos? (buffer-seq-count rest))
	  (wrap-delimited-bytes rest delimiters strip-delimiters?)))
      (more [this]
	(if-let [n (next this)] n []))
      (cons [_ bytes]
	(let [buf-seq (to-buffer-seq bytes)]
	  (wrap-delimited-bytes (concat buf-seq remainder (when first [first]) rest) delimiters strip-delimiters?))))))

(defn wrap-delimited-bytes
  ([bytes delimiters]
     (wrap-delimited-bytes bytes delimiters true))
  ([bytes delimiters strip-delimiters?]
     (let [buf-seq (to-buffer-seq bytes)
	   [x xs] (take-delimited-bytes buf-seq delimiters strip-delimiters?)
	   remainder (when-not x xs)
	   xs (when x xs)]
       (create-delimited-seq x xs remainder delimiters strip-delimiters?))))
 
