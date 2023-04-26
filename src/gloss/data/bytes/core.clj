;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^{:skip-wiki true}
  gloss.data.bytes.core
  (:require [potemkin :refer :all])
  (:import
    [java.nio
     Buffer
     ByteBuffer]))

;;;

(defn ^ByteBuffer duplicate [^ByteBuffer buf]
  (-> buf .duplicate (.order (.order buf))))

(defn ^ByteBuffer slice [^ByteBuffer buf]
  (-> buf .slice (.order (.order buf))))

(defn ^ByteBuffer rewind [^ByteBuffer buf]
  (.rewind buf))

(defn ^ByteBuffer position
  ([^ByteBuffer buf]
   (.position buf))
  ([^ByteBuffer buf n]
   (.position buf ^int n)))

(defn ^ByteBuffer limit [^ByteBuffer buf n]
  (.limit buf ^int n))

(declare create-buf-seq)

(defprotocol+ BufferSequence
  (byte-count- [this])
  (write-to-buf [this ^ByteBuffer buf])
  (rewind-bytes [this])
  (dup-bytes- [this])
  (drop-bytes- [this n])
  (take-bytes- [this n])
  (take-contiguous-bytes- [this n])
  (concat-bytes- [this bytes]))

(defn byte-count [buf-seq]
  (if buf-seq
    (byte-count- buf-seq)
    0))

(defn dup-bytes [buf-seq]
  (when buf-seq
    (dup-bytes- buf-seq)))

(defn drop-bytes [buf-seq n]
  (when buf-seq
    (drop-bytes- buf-seq n)))

(defn take-bytes [buf-seq n]
  (when buf-seq
    (take-bytes- buf-seq n)))

(defn take-contiguous-bytes [buf-seq n]
  (when buf-seq
    (take-contiguous-bytes- buf-seq n)))

(defn concat-bytes [buf-seq bytes]
  (if buf-seq
    (concat-bytes- buf-seq bytes)
    (create-buf-seq bytes)))

;;;

(deftype+ MultiBufferSequence [buf-seq byte-count]
  clojure.lang.Sequential
  clojure.lang.Seqable
  (seq [_]
    (seq buf-seq))

  clojure.lang.ISeq
  (first [this]
    (first buf-seq))

  (next [this]
    (create-buf-seq (next (seq this))))

  (more [this]
    (next this))

  (cons [this bufs]
    (create-buf-seq (concat bufs buf-seq)))

  (equiv [this other]
    (= (seq this) other))

  BufferSequence
  (byte-count- [_]
    byte-count)

  (write-to-buf [_ buf]
    (doseq [b buf-seq]
      (.put ^ByteBuffer buf ^ByteBuffer b)))

  (rewind-bytes [this]
    (doseq [b buf-seq]
      (rewind b))
    this)

  (dup-bytes- [_]
    (MultiBufferSequence. (doall (map duplicate buf-seq)) byte-count))

  (drop-bytes- [this n]
    (cond
      (not (pos? n))
      this

      (>= n byte-count)
      nil

      :else
      (create-buf-seq
        (loop [remaining n, s buf-seq]
          (when-not (empty? s)
            (let [buf ^ByteBuffer (first s)]
              (cond
                (= remaining (.remaining buf))
                (rest s)

                (< remaining (.remaining buf))
                (cons
                  (-> buf duplicate (position (+ remaining (position buf))) slice)
                  (rest s))

                :else
                (recur (- remaining (.remaining buf)) (rest s)))))))))

  (take-bytes- [this n]
    (cond
      (not (pos? n))
      nil

      (>= n byte-count)
      this

      :else
      (let [n (int n)]
        (when-let [first-buf ^ByteBuffer (first buf-seq)]
          (create-buf-seq
            (if (> (.remaining first-buf) n)
              [(-> first-buf duplicate ^ByteBuffer (.limit (+ (.position first-buf) n)) slice)]
              (when (<= n byte-count)
                (loop [remaining n, bytes buf-seq, accumulator []]
                  (if (pos? remaining)
                    (let [buf ^ByteBuffer (first bytes)]
                      (if (>= remaining (.remaining buf))
                        (recur (- remaining (.remaining buf)) (rest bytes) (conj accumulator buf))
                        (conj accumulator (-> buf duplicate ^ByteBuffer (.limit (+ (.position buf) remaining)) slice))))
                    accumulator)))))))))

  (take-contiguous-bytes- [this n]
    (let [n (int (min byte-count n))
          first-buf ^ByteBuffer (first buf-seq)]
      (if (> (.remaining first-buf) n)
        (-> first-buf duplicate ^ByteBuffer (.limit (+ (.position first-buf) n)) slice)
        (when (and (pos? n) (<= n byte-count))
          (let [ary (byte-array n)]
            (loop [offset 0, bytes buf-seq]
              (if (>= offset n)
                (ByteBuffer/wrap ary)
                (let [buf ^ByteBuffer (first bytes)
                      num-bytes (long (min (.remaining buf) (- n offset)))]
                  (-> buf duplicate (.get ary offset num-bytes))
                  (recur (+ offset num-bytes) (rest bytes))))))))))

  (concat-bytes- [_ bufs]
    (create-buf-seq (concat buf-seq bufs))))

(deftype+ SingleBufferSequence [^ByteBuffer buffer byte-count]
  clojure.lang.Sequential
  clojure.lang.Seqable
  (seq [_]
    (seq [buffer]))
  clojure.lang.ISeq
  (first [this]
    buffer)
  (next [_]
    nil)
  (more [_]
    nil)
  (cons [_ bytes]
    (create-buf-seq (concat bytes [buffer])))
  (equiv [this other]
    (and
      (sequential? other)
      (= 1 (count other))
      (= buffer (first other))))
  BufferSequence
  (byte-count- [_]
    byte-count)
  (write-to-buf [_ buf]
    (.put ^ByteBuffer buf buffer))
  (rewind-bytes [this]
    (.rewind buffer)
    this)
  (dup-bytes- [_]
    (SingleBufferSequence. (.duplicate buffer) byte-count))
  (drop-bytes- [this n]
    (cond
      (not (pos? n))
      this

      (< n byte-count)
      (let [new-position (+ n (position buffer))]
        (SingleBufferSequence. (-> buffer duplicate (position new-position) slice)
                               (- byte-count n)))

      :else
      nil))
  (take-bytes- [this n]
    (cond
      (not (pos? n))
      nil

      (< n byte-count)
      (SingleBufferSequence. (take-contiguous-bytes this n) n)

      :else
      this))
  (take-contiguous-bytes- [this n]
    (cond
      (not (pos? n))
      nil

      (<= n byte-count)
      (-> buffer duplicate (limit (min byte-count n)) slice)))
  (concat-bytes- [_ bufs]
    (create-buf-seq (cons buffer bufs))))

(defn create-buf-seq [bytes]
  (cond
    (or
      (instance? SingleBufferSequence bytes)
      (instance? MultiBufferSequence bytes))
    bytes

    (instance? ByteBuffer bytes)
    (create-buf-seq [bytes])

    (empty? bytes)
    nil

    (= 1 (count bytes))
    (SingleBufferSequence. (first bytes) (.remaining ^Buffer (first bytes)))

    :else
    (MultiBufferSequence. bytes (apply + (map #(.remaining ^Buffer %) bytes)))))
