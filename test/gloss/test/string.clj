;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns gloss.test.string
  (:require
    [clojure.test :refer :all]
    [gloss.core.formats :refer :all]
    [gloss.core.protocols :refer :all]
    [gloss.data.bytes :refer :all]
    [gloss.data.string :refer :all]))

(defn split-bytes [interval bytes]
  (let [buf-seq (to-buf-seq bytes)]
    (to-buf-seq
      (apply concat
             (map
               #(take-bytes (drop-bytes buf-seq %) 1)
               (range (byte-count buf-seq)))))))

(def ^String pilcrow-string (apply str (repeat 30 "¶")))
(def pilcrows (.getBytes pilcrow-string "utf-8"))

(defn segments [interval]
  (split-bytes interval pilcrows))

(deftest test-string-consumers
  (doseq [i (range 1 61)]
    (when (zero? (rem 60 i))
      (let [segments (split-bytes i pilcrows)
            consumer (string-codec "utf-8")]
        (is (= pilcrow-string (apply str (frame-seq consumer segments))))))))


