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

(def pilchards (.getBytes (apply str (repeat 60 "¶")) "utf-8"))

(deftest test-wrap-string
  (dotimes [i 120]
    (when (and (not (zero? i)) (zero? (rem 120 i)))
      (let [segments (split-str i pilchards)]
	(= pilchards (str (reduce concat-bytes (wrap-string (first segments)) (rest segments))))))))


