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
     ByteBuffer
     CharBuffer]))

(defn buf-seq-count [buf-seq]
  (apply + (map #(.remaining ^Buffer %) buf-seq)))

;;;

(defn write-buf-seq [buf-seq buf]
  (doseq [b buf-seq]
    (.put ^ByteBuffer buf ^ByteBuffer b)))

(defn rewind-buf-seq [buf-seq]
  (concat
    (map #(.rewind ^ByteBuffer %) (drop-last buf-seq))
    (let [last-buf ^ByteBuffer (last buf-seq)
	  last-pos (.position last-buf)]
      [(-> last-buf (.position 0) ^ByteBuffer (.limit last-pos) .slice)])))

(defn dup-buf-seq [buf-seq]
  (map #(.duplicate ^ByteBuffer %) buf-seq))

(defn drop-from-bufs
  [n buf-seq]
  (cond
    (not (pos? n))
    buf-seq

    (>= n (buf-seq-count buf-seq))
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
	      (-> buf .duplicate ^ByteBuffer (.position (+ remaining (.position buf))) .slice)
	      (rest s))

	    :else
	    (recur (- remaining (.remaining buf)) (rest s))))))))

(defn take-from-bufs
  [n buf-seq]
  (cond
    (not (pos? n))
    nil

    (>= n (buf-seq-count buf-seq))
    buf-seq

    :else
    (when-let [first-buf ^ByteBuffer (first buf-seq)]
      (if (> (.remaining first-buf) n)
	[(-> first-buf .duplicate ^ByteBuffer (.limit (+ (.position first-buf) n)) .slice)]
	(when (<= n (buf-seq-count buf-seq))
	  (loop [remaining n, s buf-seq, accumulator []]
	    (if (pos? remaining)
	      (let [buf ^ByteBuffer (first s)]
		(if (>= remaining (.remaining buf))
		  (recur (- remaining (.remaining buf)) (rest s) (conj accumulator buf))
		  (conj accumulator (-> buf .duplicate ^ByteBuffer (.limit (+ (.position buf) remaining)) .slice))))
	      accumulator)))))))

;;; TODO: macro these away

(defn rewind-char-buf-seq [buf-seq]
  (concat
    (map #(.rewind ^CharBuffer %) (drop-last buf-seq))
    (let [last-buf ^CharBuffer (last buf-seq)
	  last-pos (.position last-buf)]
      [(-> last-buf (.position 0) ^CharBuffer (.limit last-pos) .slice)])))

(defn drop-from-char-bufs
  [n buf-seq]
  (cond
    (not (pos? n))
    buf-seq

    (>= n (buf-seq-count buf-seq))
    nil

    :else
    (loop [remaining n, s buf-seq]
      (when-not (empty? s)
	(let [buf ^CharBuffer (first s)]
	  (cond
	    (= remaining (.remaining buf))
	    (rest s)

	    (< remaining (.remaining buf))
	    (cons
	      (-> buf .duplicate ^CharBuffer (.position (+ remaining (.position buf))) .slice)
	      (rest s))

	    :else
	    (recur (- remaining (.remaining buf)) (rest s))))))))

(defn take-from-char-bufs
  [n buf-seq]
  (cond
    (not (pos? n))
    nil

    (>= n (buf-seq-count buf-seq))
    buf-seq

    :else
    (when-let [first-buf ^CharBuffer (first buf-seq)]
      (if (> (.remaining first-buf) n)
	[(-> first-buf .duplicate ^CharBuffer (.limit (+ (.position first-buf) n)) .slice)]
	(when (<= n (buf-seq-count buf-seq))
	  (loop [remaining n, s buf-seq, accumulator []]
	    (if (pos? remaining)
	      (let [buf ^CharBuffer (first s)]
		(if (>= remaining (.remaining buf))
		  (recur (- remaining (.remaining buf)) (rest s) (conj accumulator buf))
		  (conj accumulator (-> buf .duplicate ^CharBuffer (.limit (+ (.position buf) remaining)) .slice))))
	      accumulator)))))))

;;;

(defn drop-bytes
  "Returns the buffer-sequence less the first 'n' bytes."
  [n bytes]
  (let [buf-seq (to-buf-seq bytes)]
    (drop-from-bufs n buf-seq)))

(defn take-contiguous-bytes
  "Returns a single ByteBuffer which contains the first 'n' bytes of the buffer-sequence."
  [n buf-seq]
  (when-not (or (empty? buf-seq) (< (buf-seq-count buf-seq) n))
    (let [buf-seq (dup-buf-seq buf-seq)
	  first-buf ^ByteBuffer (first buf-seq)]
      (if (> (.remaining first-buf) n)
	(-> first-buf .duplicate ^ByteBuffer (.limit (+ (.position first-buf) n)) .slice)
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

;;;

(defn- first-byte [s]
  (let [buf ^ByteBuffer (first s)]
    (.get buf (.position buf))))

(defn- advance-buf-seq [s]
  (let [buf ^ByteBuffer (first s)
	buf (.position buf (inc (.position buf)))]
    (if (.hasRemaining buf)
      s
      (rest s))))

(defn- match-delimiters [delimiters bytes]
  (some
    (fn [^ByteBuffer delimiter]
      (and
	(= (.get delimiter 0) (first-byte bytes))
	(or
	  (= 1 (.remaining delimiter))
	  (= delimiter (take-contiguous-bytes (.remaining delimiter) bytes)))
	delimiter))
    (dup-buf-seq delimiters)))

;;naive solution - this assumes small and dissimilar delimiters
(defn take-delimited-bytes
  "Returns the buffer-sequence as two buffer-sequences, split around the first found
   instance of a delimiter."
  ([bytes delimiters]
     (take-delimited-bytes delimiters bytes true))
  ([bytes delimiters strip-delimiters?]
     (let [buf-seq (to-buf-seq bytes)
	   delimiters (->> delimiters
			dup-buf-seq
			(sort #(compare (.remaining ^ByteBuffer %1) (.remaining ^ByteBuffer %2)))
			reverse)]
       (loop [s (dup-buf-seq buf-seq)]
	 (if (empty? s)
	   [false nil buf-seq]
	   (if-let [delimiter ^ByteBuffer (match-delimiters delimiters s)]
	     (let [delimiter-count (-> delimiter .rewind .remaining)
		   location (- (buf-seq-count buf-seq) (buf-seq-count s))]
	       [true
		(take-bytes (+ location (if strip-delimiters? 0 delimiter-count)) buf-seq)
		(drop-bytes (+ location delimiter-count) buf-seq)])
	     (recur (advance-buf-seq s))))))))

;;;

(defn take-all [codec]
  (fn [byte-seq remainder]
    (loop [byte-seq byte-seq, s []]
      (if (empty? byte-seq)
	[true s remainder]
	(let [[success v b] (read-bytes codec byte-seq)]
	  (when-not success
	    (throw (Exception. "Cannot evenly divide byte-sequence")))
	  (recur b (conj s v)))))))

(defn finite-byte-codec
  [len]
  (reify
    Reader
    (read-bytes [this b]
      (if (< (buf-seq-count b) len)
	[false this b]
	[true (take-bytes len b) (drop-bytes len b)]))
    Writer
    (sizeof [_]
      len)
    (write-bytes [_ _ v]
      v)))

;;;

(defn delimited-byte-codec
  ([delimiters strip-delimiters?]
     (delimited-byte-codec
       nil
       (map to-byte-buffer delimiters)
       strip-delimiters?))
  ([scanned delimiters strip-delimiters?]
     (let [delimiters (dup-buf-seq delimiters)
	   max-delimiter-size (apply max
				(map
				  #(.remaining ^ByteBuffer %)
				  delimiters))
	   split-index (- (buf-seq-count scanned) (dec max-delimiter-size))
	   prefix (drop-bytes split-index scanned)
	   scanned (take-bytes split-index scanned)]
       (reify
	 Reader
	 (read-bytes [this b]
	   (let [[success x xs] (take-delimited-bytes (concat prefix b) delimiters strip-delimiters?)]
	     (if success
	       [true (concat scanned x) xs]
	       [false (delimited-byte-codec (concat scanned prefix b) delimiters strip-delimiters?) nil])))
	 Writer
	 (sizeof [_]
	   nil)
	 (write-bytes [_ _ v]
	   (concat v [(.duplicate ^ByteBuffer (first delimiters))]))))))

(defn delimited-block
  [codec delimiters]
  (let [delimiters (dup-buf-seq delimiters)
	inner-codec (delimited-byte-codec delimiters true)
	delimited-codec (compose-readers
			  inner-codec
			  (fn [bytes remainder]
			    (let [[success v remainder*] (read-bytes codec bytes)]
			      (assert success)
			      (assert (empty? remainder*))
			      [true v remainder])))]
    (reify
      Reader
      (read-bytes [_ b]
	(read-bytes delimited-codec b))
      Writer
      (sizeof [_]
	nil)
      (write-bytes [_ buf v]
	(concat
	  (if (sizeof codec)
	    (with-buffer [buf (sizeof codec)]
	      (write-bytes codec buf v))
	    (write-bytes codec buf v))
	  [(.duplicate ^ByteBuffer (first delimiters))])))))

(defn wrap-delimited-sequence
  [codec delimiters]
  (let [delimiters (dup-buf-seq delimiters)
	suffix (first delimiters)
	sizeof-delimiter (.remaining ^Buffer suffix)
	read-codec (compose-readers 
		     (delimited-byte-codec delimiters true)
		     (take-all codec))]
    (reify
      Reader
      (read-bytes [_ b]
	(read-bytes read-codec b))
      Writer
      (sizeof [_]
	nil)
      (write-bytes [_ buf vs]
	(if (sizeof codec)
	  (with-buffer [buf (+ sizeof-delimiter (* (count vs) (sizeof codec)))]
	    (doseq [v vs]
	      (write-bytes codec buf v))
	    (.put ^ByteBuffer buf (.duplicate ^ByteBuffer suffix)))
	  (apply concat
	    (map #(write-bytes codec buf %) vs)
	    [(.duplicate ^ByteBuffer suffix)]))))))

 
