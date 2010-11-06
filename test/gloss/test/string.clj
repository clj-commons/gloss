;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns gloss.test.string
  (:use
    [gloss core string]
    [clojure test]))

(defn split-str [interval bytes]
  (let [buf-seq (to-buffer-seq bytes)]
    (apply concat (map #(take-bytes 1 (drop-bytes % buf-seq)) (range (buffer-seq-count buf-seq))))))

(def pilchards-string (apply str (repeat 30 "¶")))
(def pilchards (.getBytes pilchards-string "utf-8"))

(defn segments [interval]
  (split-str interval pilchards))

(defn concat-and-verify [f]
  (fn [s b]
    (is (= s (f s)))
    (concat-bytes s b)))

(deftest test-wrap-string
  (doseq [i (range 1 61)]
    (when (zero? (rem 60 i))
      (let [segments (split-str i pilchards)]
	(is (= pilchards-string (str (reduce concat-bytes (wrap-string (first segments)) (rest segments)))))))))

(deftest test-wrap-string-sequence
  (let [divisors (filter #(zero? (rem 30 %)) (range 1 61))]
    (doseq [buf-interval divisors]
      (doseq [string-interval divisors]
	(let [test-sequence (fn [s] (every? #(= % (first "¶")) (apply str s)))]
	  (is (every? #(= string-interval (count %)) (wrap-string-sequence (segments buf-interval) string-interval)))
	  (is (= 30 (count (apply str (wrap-string-sequence (segments string-interval) buf-interval)))))
	  (let [segments (segments buf-interval)
		string-seq (reduce concat-bytes (wrap-string-sequence (first segments) string-interval) (rest segments))]
	    (is (= pilchards-string (apply str string-seq)))))))))


