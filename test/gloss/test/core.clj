;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns gloss.test.core
  (:use
    [gloss core io]
    [gloss.core.formats :only (to-char-buffer to-buf-seq)]
    [gloss.core.protocols :only (write-bytes read-bytes)]
    [gloss.data.bytes :only (take-bytes drop-bytes dup-bytes byte-count take-contiguous-bytes)]
    [lamina core]
    [clojure test walk]))

(defn convert-char-sequences [x]
  (postwalk #(if (instance? CharSequence %) (str %) %) x))

(defn split-bytes [interval bytes]
  (let [buf-seq (to-buf-seq bytes)]
    (apply concat (map #(take-bytes 1 (drop-bytes % buf-seq)) (range (byte-count buf-seq))))))

(defn compare-result [expected result]
  (is (= expected (convert-char-sequences (second result))))
  (is (= true (first result)))
  (is (empty? (nth result 2))))

(defn test-stream-roundtrip [frame vals]
  (let [bytes (split-bytes 1 (encode frame vals))
	ch (decoder-channel frame)]
    (doseq [b bytes]
      (enqueue ch b))
    (let [s (convert-char-sequences (channel-seq ch))]
      (if (= 1 (count s))
	(is (= vals (first s)))
	(is (= vals (apply str s)))))))

(defn test-roundtrip [f val]
  (let [f (compile-frame f)
	bytes (write-bytes f nil val)
	result (read-bytes f (dup-bytes bytes))
	split-result (read-bytes f (split-bytes 1 (dup-bytes bytes)))
	]
    (test-stream-roundtrip f val)
    (compare-result val result)
    (compare-result val split-result)
    ))

(defn test-full-roundtrip [f buf val]
  (compare-result val (read-bytes f (dup-bytes buf)))
  (is (= buf (write-bytes f nil val))))

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
    (range 100))
  (test-roundtrip
    (repeated [:byte :byte])
    (partition 2 (range 100)))
  (test-roundtrip
    (repeated :byte :delimiters [64])
    (range 10))
  (test-roundtrip
    (repeated {:a :int32 :b :int32})
    (repeat 100 {:a 1 :b 2}))
  (test-roundtrip
    (repeated :int32 :prefix (prefix :byte))
    (range 100)))

(deftest test-complex-prefix
  (let [p (prefix [:byte :byte]
	    second
	    (fn [x] [\$ x]))
	codec (repeated :byte :prefix p)
	buf (to-byte-buffer [\$ 3 1 2 3])]
    (test-full-roundtrip codec [buf] [1 2 3])))

(deftest test-simple-header
  (let [b->h (fn [body]
	       (get
		 {:a 1 :b 2 :c 3}
		 (first body)))
	h->b (fn [hd]
	       (condp = hd
		 1 (compile-frame [:a :int16])
		 2 (compile-frame [:b :float32])
		 3 (compile-frame [:c (string :utf-8 :delimiters [\0])])))
	codec (header :byte h->b b->h)]
    (test-roundtrip codec [:a 1])
    (test-roundtrip codec [:b 2.5])
    (test-roundtrip codec [:c "abc"])))

(deftest test-enum
  (test-roundtrip
    (enum :a :b :c)
    :a)
  (test-roundtrip
    (enum {:a 100 :b 1000})
    :b))

(deftest test-string
  (test-roundtrip
    (string :utf-8)
    "abcd")
  (test-roundtrip
    (repeated (string :utf-8 :delimiters ["\0"]))
    ["abc" "def"])
  (test-roundtrip
    [:a (string :utf-8 :delimiters ["xyz"])]
    [:a "abc"]))
