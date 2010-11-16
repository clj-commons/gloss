;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns gloss.test.string
  (:use
    [gloss.data bytes string]
    [gloss.core protocols formats]
    [clojure test]))

(defn split-str [interval bytes]
  (let [buf-seq (to-buf-seq bytes)]
    (apply concat (map #(take-bytes 1 (drop-bytes % buf-seq)) (range (buf-seq-count buf-seq))))))

(def pilchards-string (apply str (repeat 30 "¶")))
(def pilchards (.getBytes pilchards-string "utf-8"))

(defn segments [interval]
  (split-str interval pilchards))

(deftest test-string-consumer
  (doseq [i (range 1 61)]
    (when (zero? (rem 60 i))
      (let [segments (split-str i pilchards)
	    consumer (string-codec "utf-8")]
	(is (= pilchards-string (apply str (frame-seq consumer segments))))))))

(deftest test-finite-string-consumer
  (let [divisors (filter #(zero? (rem 30 %)) (range 1 61))
	pilchar (first "¶")]
    (doseq [buf-interval divisors]
      (doseq [string-interval divisors]
	(let [strs (frame-seq (finite-string-codec "utf-8" string-interval) (split-str buf-interval pilchards))]
	  (is (every? #(= string-interval (count %)) strs))
	  (is (= 30 (count (apply str strs)))))))))


