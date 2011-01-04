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

(defn split-bytes [interval bytes]
  (let [buf-seq (to-buf-seq bytes)]
    (to-buf-seq
      (apply concat
	(map
	  #(take-bytes (drop-bytes buf-seq %) 1)
	  (range (byte-count buf-seq)))))))

(def pilchards-string (apply str (repeat 30 "¶")))
(def pilchards (.getBytes pilchards-string "utf-8"))

(defn segments [interval]
  (split-bytes interval pilchards))

(deftest test-string-consumers
  (doseq [i (range 1 61)]
    (when (zero? (rem 60 i))
      (let [segments (split-bytes i pilchards)
	    consumer (string-codec "utf-8")]
	(is (= pilchards-string (apply str (frame-seq consumer segments))))))))


