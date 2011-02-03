;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^{:skip-wiki true}
  gloss.data.bytes.bits
  (:use
    [gloss.core protocols]
    [gloss.data.bytes core])
  (:import
    [java.math BigInteger]
    [java.nio ByteBuffer]))

(defn ^BigInteger bit-range [num-bytes start end]
  (reduce
    #(.setBit ^BigInteger %1 %2)
    (BigInteger. (byte-array num-bytes))
    (range start end)))

(defn to-bool [x]
  (if (number? x)
    (not (zero? x))
    x))

(defn bit-seq
  "Defines a sequence of signed integers with the specified bit-lengths.  The sum of the
   bit-lengths must be divisable by 8.  Single bit values are treated specially, and will
   decode to simply 'true' or 'false'.

   (bit-seq 4 3 1) <=> [7 -3 true]

   If a number is larger than its bit-count can contain, it will be truncated during encoding."
  [& bit-lengths]
  (let [total-length (apply + bit-lengths)]
    (when-not (zero? (rem total-length 8))
      (throw (Exception. (str "Total length of " total-length " not divisable by 8."))))
    (let [byte-length (/ total-length 8)
	  bit-offsets (reductions + 0 bit-lengths)
	  bit-masks (doall(map
			    #(bit-range byte-length %1 (+ %1 %2))
			    bit-offsets
			    bit-lengths))]
      (reify
	Reader
	(read-bytes [this b]
	  (if (< (byte-count b) byte-length)
	    [false this b]
	    (let [ary (byte-array byte-length)]
	      (-> b (take-contiguous-bytes byte-length) (.get ary))
	      (let [bits (BigInteger. ary)]
		[true
		 (doall
		   (map
		     #(let [val ^BigInteger (-> ^BigInteger %1 (.and %2) (.shiftRight %3))]
			(if (= 1 %4)
			  (.testBit val 0)
			  (.intValue val)))
		     (repeat bits)
		     bit-masks
		     bit-offsets
		     bit-lengths))
		 (drop-bytes b byte-length)]))))
	Writer
	(sizeof [_] byte-length)
	(write-bytes [this buf vals]
	  (with-buffer [^ByteBuffer buf byte-length]
	    (let [vals (map
			 #(let [val ^BigInteger
				(if (= 1 %4)
				  (if (to-bool %1)
				    (.setBit (BigInteger/ZERO) 1)
				    (BigInteger/ZERO))
				  (BigInteger/valueOf %1))]
			    (.and ^BigInteger %3 (.shiftLeft val %2)))
			 vals
			 bit-offsets
			 bit-masks
			 bit-lengths)]
	      (let [ary (.toByteArray ^BigInteger (reduce #(.or ^BigInteger %1 %2) vals))
		    pos (.position buf)]
		(.put ^ByteBuffer buf ary 0 (min (count ary) byte-length))
		(.position buf (+ pos byte-length))))))))))


