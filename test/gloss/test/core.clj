;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns gloss.test.core
  (:use
    [gloss.core]
    [clojure test]))

(defn test-roundtrip [f val]
  (let [f (compile-frame f)
	bytes (write-bytes f nil val)
	result (read-bytes f bytes)]
    (is (= val (second result)))))

(defn test-transformed-roundtrip [f transform val]
  (let [f (compile-frame f)]
    (is (= val (transform (second (read-bytes f (write-bytes f nil val))))))))

(deftest test-lists
  (test-roundtrip
    [:float32 :float32]
    [1 2])
  (test-roundtrip
    [:a :byte :float64 :b]
    [:a 1 2 :b])
  (test-roundtrip
    [:int16 [:int32 [:int64]]]
    [1 [2 [3]]]))

(deftest test-maps
  (test-roundtrip
    {:a :int32 :b :int32}
    {:a 1 :b 2})
  (test-roundtrip
    {:a :int32 :b [:int32 :int32]}
    {:a 1 :b [2 3]})
  (test-roundtrip
    {:a :int32 :b {:c {:d :int16}}}
    {:a 1 :b {:c {:d 2}}})
  (test-roundtrip
    [{:a :int32} {:b [:float64 :float32]}]
    [{:a 1} {:b [2 3]}]))

(deftest test-repeated
  (test-roundtrip
    (repeated :int32)
    (range 1000))
  (test-roundtrip
    (repeated [:byte :byte])
    (partition 2 (range 100)))
  (test-roundtrip
    (repeated :byte :delimiters [127])
    (range 100)))

(deftest test-string
  (test-transformed-roundtrip
    (string :utf-8)
    str
    "abcd")
  (test-transformed-roundtrip
    (repeated (string :utf-8 :delimiters ["\0"]))
    #(map str %)
    ["abc" "def"]))
