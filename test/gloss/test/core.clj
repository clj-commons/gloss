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
    [gloss.core.formats :only (to-char-buffer)]
    [gloss.core.protocols :only (write-bytes read-bytes)]
    [gloss.data.bytes :only (take-bytes drop-bytes dup-bytes take-contiguous-bytes)]
    [lamina core]
    [clojure test walk]))

(defn convert-char-sequences [x]
  (postwalk
    #(if (instance? CharSequence %)
       (str %)
       %)
    x))

(defn convert-buf-seqs [x]
  (postwalk
    #(if (and (sequential? %) (instance? java.nio.ByteBuffer (first %)))
       [(contiguous %)]
       %)
    x))

(defmacro big-int? [x]
  (if (= 2 (:minor *clojure-version*))
    false
    `(instance? clojure.lang.BigInt ~x)))

(defn normalize-number [x]
  (if (big-int? x)
    x
    (double x)))

(defn convert-string-sequence [x]
  (if (and (sequential? x) (every? string? x))
    (apply str x)
    (postwalk #(if (number? %) (normalize-number %) %) x)))

(defn convert-result [x]
  (-> x convert-buf-seqs convert-char-sequences convert-string-sequence))

(defn is= [a b]
  (is
    (= (convert-string-sequence a) (convert-result b))
    (str (prn-str a) (prn-str b))))

(defn partition-bytes [interval bytes]
  (let [buf-seq (to-buf-seq bytes)]
    (to-buf-seq
      (apply concat
	(map
	  #(take-bytes (drop-bytes buf-seq %) 1)
	  (range (byte-count buf-seq)))))))

(defn split-bytes [index bytes]
  (let [bytes (-> bytes to-buf-seq dup-bytes)]
    [(take-bytes bytes index) (drop-bytes bytes index)]))

(defn split-channel [split-fn frame val]
  (apply closed-channel (split-fn (encode-all frame [val val]))))

(defn test-stream-roundtrip [split-fn frame val]
  (let [ch (decode-channel (split-channel split-fn frame val) frame)]
    (let [s (convert-result (channel-seq ch))]
      (is= [val val] s)))
  (let [ch (decode-channel-headers (split-channel split-fn frame val) frame)
	v1 (wait-for-message ch)
	v2 (->> (decode-channel ch frame) channel-seq)]
    (let [s (convert-result (cons v1 v2))]
      (is= [val val] s))))

(defn test-simple-roundtrip [f val]
  (let [bytes (encode f val)
	val* (convert-char-sequences (decode f bytes))]
    (is (= val val*))))

(defn test-roundtrip [f val]
  (let [f (compile-frame f)
	bytes (encode f val)
	val* (convert-char-sequences (decode f bytes))
	bytes (encode-all f [val val])
	result (decode-all f bytes)
	contiguous-result (decode-all f (contiguous bytes))
	split-result (->> bytes to-buf-seq dup-bytes (partition-bytes 1) (decode-all f))
	lazy-result (lazy-decode-all f bytes)
        lazy-contiguous-result (lazy-decode-all f bytes)
        lazy-split-result (->> bytes to-buf-seq dup-bytes (partition-bytes 1) (lazy-decode-all f))]
    (is= val val*)
    (test-stream-roundtrip #(partition-bytes 1 %) f val)
    (is= [val val] result)
    (is= [val val] split-result)
    (is= [val val] contiguous-result)
    (is= [val val] lazy-result)
    (is= [val val] lazy-split-result)
    (is= [val val] lazy-contiguous-result)
    (doseq [i (range 1 (byte-count bytes))]
      (is= [val val] (decode-all f (apply concat (split-bytes i bytes))))
      (test-stream-roundtrip #(split-bytes i %) f val)
      )))

(defn test-full-roundtrip [f buf val]
  (is= val (decode f (-> buf to-buf-seq dup-bytes)))
  (is (= buf (write-bytes f nil val))))

;;;

(deftest test-lists
  (test-roundtrip
    [:float32-be :float32-le]
    [1 2])
  (test-roundtrip
    [:ubyte :uint16-le :uint32 :uint64-be]
    [0xFF 0xFFFF 0xFFFFFFFF 0xFFFFFFFFFFFFFFFF])
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
    (range 10))
  (test-roundtrip
    (repeated [:byte :byte])
    (partition 2 (range 100)))
  (test-roundtrip
    (repeated :byte :delimiters [64])
    (range 10))
  (test-roundtrip
    (repeated (string :utf-8 :delimiters ["/n"]) :delimiters ["/0"])
    ["foo" "bar" "baz"])
  (test-roundtrip
    (repeated {:a :int32 :b :int32})
    (repeat 10 {:a 1 :b 2}))
  (test-roundtrip
    (repeated :int32 :prefix (prefix :byte))
    (range 10))
  (test-roundtrip
    (repeated :byte :prefix :int32)
    (range 10))
  (test-roundtrip
    (finite-frame (prefix :int16)
      (repeated :int32 :prefix :none))
    (range 10))
  (test-roundtrip
    [:byte (repeated :int32)]
    [1 [2]]))

(deftest test-finite-block
  (test-roundtrip
    [:byte :int16
     (finite-block
       (prefix :int64
	 #(- % 4)
	 #(+ % 4)))
     (finite-block 3)
     {:abc (finite-block 3)}]
    [1
     1
     (encode (repeated :int16) (range 5))
     (encode (string :ascii) "abc")
     {:abc (encode (string :ascii) "abc")}]))

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
    (enum :byte :a :b :c)
    :a)
  (test-roundtrip
    (enum :int16 {:a 100 :b 1000 :c \c})
    :c))

(deftest test-bit-seq
  (test-roundtrip
    (bit-seq 4 4)
    [1 15])
  (test-roundtrip
    (bit-seq 6 1 1)
    [31 true false])
  (test-roundtrip
    (apply bit-seq (range 1 16))
    (cons false (rest (range 1 16))))
  (test-roundtrip
    (apply bit-seq (range 1 16))
    (cons false (take 14 (repeat 0))))
  (test-roundtrip
    [:int32 (bit-seq 4 4 4 4) :float32]
    [1 [13 12 1 5] 6.0]))

(deftest test-bit-map
  (test-roundtrip
    (bit-map :a 4 :b 4)
    {:a 1 :b 15})
  (test-roundtrip
    (bit-map :a 7 :b 1)
    {:a 63, :b false}))

(deftest test-string
  (test-roundtrip
    (string :utf-8)
    "abcd")
  (test-roundtrip
    (repeated (string :utf-8 :delimiters ["\0"]))
    ["abc" "def"])
  (test-roundtrip
    [:a (string :utf-8 :delimiters ["xy" "xyz"])]
    [:a "abc"])
  (test-roundtrip
    (string :utf-8 :length 3)
    "foo")
  (test-roundtrip
    (string :utf-8 :length 3 :suffix "z")
    "foo")
  (test-roundtrip
    (string :utf-8 :suffix "z" :delimiters ["\r\n"])
    "foobar")
  (test-roundtrip
    (finite-frame 5 (string :utf-8))
    "abcde"))

(deftest test-string-numbers
  (test-roundtrip
    (repeated (string-integer :utf-8 :length 5))
    [12345 67890 123 45])
  (test-roundtrip
    (repeated (string-float :utf-8 :length 5))
    [123.4 67.89 1.23 4.5])
  (test-roundtrip
    (repeated (string-integer :ascii :delimiters ["x"]))
    [1 23 456 7890])
  (test-roundtrip
    (repeated (string-float :ascii :delimiters ["x"]))
    [(/ 3 2) 1.5 0.66666])
  (test-roundtrip
    (repeated :int32
      :prefix (prefix (string-integer :ascii :delimiters ["x"])))
    [1 2 3]))

(deftest test-ordered-map
  (test-roundtrip
    (ordered-map :b :int32 :a :int32)
    {:a 1 :b 2})
  (test-roundtrip
    (ordered-map :b :int32 :a [:int32 :int32])
    {:a [2 3] :b 1}))

(deftest test-netstrings
  (test-roundtrip
    (finite-frame
      (prefix (string-integer :ascii :delimiters [":"])
	inc
	dec)
      (string :utf-8 :suffix ","))
    "abc"))
