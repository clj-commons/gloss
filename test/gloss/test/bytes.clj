;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns gloss.test.bytes
  (:use
    [gloss.data bytes]
    [gloss.data.bytes.delimited :only (delimited-bytes-splitter)]
    [gloss.core formats])
  (:use [clojure test])
  (:import [java.nio ByteBuffer]))

(defn byte-seq [^ByteBuffer buf]
  (let [buf (duplicate buf)]
    (lazy-seq
      (when (.hasRemaining buf)
	(cons (.get buf) (byte-seq buf))))))

(defn- gen-test-bufs []
  [;; single buffer
   (to-buf-seq (to-byte-buffer (range 100)))
   ;; single buffer - non-zero position
   (to-buf-seq (.position (to-byte-buffer (range -2 100)) 2))
   ;; multi buffer
   (to-buf-seq (map to-byte-buffer (partition 5 (range 100))))
   ;; multi buffer - non-zero position
   (to-buf-seq (map #(.position (to-byte-buffer %) 2)
                    (partition 7 5 (range -2 100))))])

(deftest test-drop-bytes
  (let [[buf buf-pos bufs bufs-pos] (gen-test-bufs)]
    (dotimes [i 101]
      (is (= (drop i (range 100)) (mapcat byte-seq (drop-bytes buf i))))
      (is (= (drop i (range 100)) (mapcat byte-seq (drop-bytes buf-pos i))))
      (is (= (drop i (range 100)) (mapcat byte-seq (drop-bytes bufs i))))
      (is (= (drop i (range 100)) (mapcat byte-seq (drop-bytes bufs-pos i)))))))

(deftest test-take-bytes
  (let [[buf buf-pos bufs bufs-pos] (gen-test-bufs)]
    (dotimes [i 101]
      (is (= (take i (range 101)) (mapcat byte-seq (take-bytes buf i))))
      (is (= (take i (range 101)) (mapcat byte-seq (take-bytes buf-pos i))))
      (is (= (take i (range 101)) (mapcat byte-seq (take-bytes bufs i))))
      (is (= (take i (range 101)) (mapcat byte-seq (take-bytes bufs-pos i)))))))

(deftest test-take-contiguous-bytes
  (let [[buf buf-pos bufs bufs-pos] (gen-test-bufs)]
    (dotimes [i 101]
      (is (= (take i (range 100)) (byte-seq (take-contiguous-bytes buf i))))
      (is (= (take i (range 100)) (byte-seq (take-contiguous-bytes buf-pos i))))
      (is (= (take i (range 100)) (byte-seq (take-contiguous-bytes bufs i))))
      (is (= (take i (range 100)) (byte-seq (take-contiguous-bytes bufs-pos i)))))))

(defn- test-split [split-location skip-bytes source split]
  (let [s (mapcat byte-seq source)
	split (map #(mapcat byte-seq %) (rest split))]
    (if (<= split-location (count s))
      (is
	(= [(take split-location s) (drop (+ split-location skip-bytes) s)] split)
	(with-out-str (prn split-location skip-bytes (map byte-seq source))))
      (is (= [() s] split)))))

(defn take-delimited-bytes [buf-seq delimiters strip-delimiters?]
  (let [f (delimited-bytes-splitter delimiters strip-delimiters?)]
    (f buf-seq)))

(deftest test-take-delimited-bytes
  ;;single-byte delimiters
  (let [bufs (to-buf-seq (map to-byte-buffer (partition 3 (range 12))))]
    (dotimes [i 12]
      (let [delimiters [(to-byte-buffer [i])]]
	(test-split i 1 bufs (take-delimited-bytes bufs delimiters true))
	(test-split (inc i) 0 bufs (take-delimited-bytes bufs delimiters false)))))

  ;;non-existent delimiters
  (let [bufs (to-buf-seq (map to-byte-buffer (partition 4 (range 100))))]
    (test-split 101 0 bufs (take-delimited-bytes bufs [(to-byte-buffer [1 3])] false))
    (test-split 101 0 bufs (take-delimited-bytes bufs [(to-byte-buffer [101])] false)))

  ;;multi-byte delimiters
  (let [bufs (to-buf-seq (map to-byte-buffer (partition 1 (range 15))))]
    (dotimes [i 11]
      (let [delimiters (map #(to-byte-buffer (range i (+ i %))) (range 1 5))]
	(test-split (+ i 4) 0 bufs (take-delimited-bytes bufs delimiters false))
	(test-split i 4 bufs (take-delimited-bytes bufs delimiters true))))))
