;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^{:skip-wiki true}
  gloss.data.bytes.bits
  (:require
    [gloss.core.protocols :refer [Reader Writer with-buffer]]
    [gloss.data.bytes.core :refer :all])
  (:import
    [java.math BigInteger]
    [java.nio ByteBuffer]))

(defn print-bits [^BigInteger b]
  (println (.bitLength b) (map #(if (.testBit b %) 1 0) (range (.bitLength b)))))

(defn to-bool [x]
  (if (number? x)
    (not (zero? x))
    x))

(defn ^BigInteger bit-range [num-bytes start end]
  (reduce
    #(.setBit ^BigInteger %1 %2)
    (BigInteger. (byte-array num-bytes))
    (range start end)))

(defn ^BigInteger buf-seq->big-integer [buf byte-length]
  (let [ary (byte-array byte-length)]
    (-> buf ^ByteBuffer (take-contiguous-bytes byte-length) (.get ary))
    (BigInteger. ary)))

(defn bit-mask-reader [offset length]
  (let [mask (bit-range length offset (+ offset length))]
    (fn [^BigInteger n]
      (let [val (-> n (.and mask) (.shiftRight offset))]
        (if (= 1 length)
          (.testBit val 0)
          (.intValue val))))))

(defn bit-mask-writer [offset length]
  (if (= 1 length)
    (fn [^BigInteger n val]
      (if (to-bool val)
        (.setBit n offset)
        n))
    (fn [^BigInteger n val]
      (.or n (.shiftLeft (BigInteger/valueOf val) offset)))))

(defn bit-seq
  "Defines a sequence of unsigned integers with the specified bit-lengths.  The sum of the
   bit-lengths must be divisable by 8.  Single bit values are treated specially, and will
   decode to simply 'true' or 'false'.

   (bit-seq 4 3 1) <=> [15 7 true]

   If a number is larger than its bit-count can contain, it will be truncated during encoding."
  [& bit-lengths]
  (let [total-length (apply + bit-lengths)]
    (when-not (zero? (rem total-length 8))
      (throw (Exception. (str "Total bit-length of " total-length " not divisable by 8."))))
    (let [byte-length (int (/ total-length 8))
          bit-offsets (->> bit-lengths
                           (reductions + 0)
                           butlast
                           (map #(- total-length %))
                           (map #(- %2 %1) bit-lengths))
          readers (map bit-mask-reader bit-offsets bit-lengths)
          writers (map bit-mask-writer bit-offsets bit-lengths)]
      (reify
        Reader
        (read-bytes [this b]
          (if (< (byte-count b) byte-length)
            [false this b]
            [true
             (doall (map #(%1 %2) readers (repeat (buf-seq->big-integer b byte-length))))
             (drop-bytes b byte-length)]))
        Writer
        (sizeof [_] byte-length)
        (write-bytes [this buf vals]
          (with-buffer [^ByteBuffer buf byte-length]
                       (let [ary (->> vals
                                      (map vector writers)
                                      ^BigInteger
                                      (reduce
                                        (fn [^BigInteger n [writer val]]
                                          (writer n val))
                                        (BigInteger/ZERO))
                                      .toByteArray)
                             pos (.position ^ByteBuffer buf)
                             cnt (count ary)]
                         (when (< cnt byte-length)
                           (.position ^ByteBuffer buf (+ pos (- byte-length cnt))))
                         (.put ^ByteBuffer buf
                               ary
                               (max 0 (- cnt byte-length))
                               (min cnt byte-length))
                         (.position ^ByteBuffer buf (+ pos byte-length)))))))))


