;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns gloss.test.core
  (:require
    [clojure.test :refer :all]
    [clojure.walk :refer :all]
    [gloss.core :refer :all]
    [gloss.io :refer :all]
    [gloss.core.formats :refer [to-char-buffer]]
    [gloss.core.protocols :refer [write-bytes read-bytes]]
    [gloss.data.bytes :refer [take-bytes drop-bytes dup-bytes take-contiguous-bytes buf->string]]
    [manifold.stream :as s]
    [manifold.deferred :as d])
  (:import (java.nio ByteBuffer)))

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

(defn is=
  ([a b]
   (is= a b (str (prn-str a) (prn-str b))))
  ([a b mesg]
   (is (= (convert-string-sequence a) (convert-result b)) mesg)))

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

(defn split-stream [split-fn frame val]
  (s/->source (split-fn (encode-all frame [val val]))))

(defn test-stream-roundtrip [split-fn frame val mesg]
  (let [ch (decode-stream (split-stream split-fn frame val) frame)]
    (let [s (convert-result (s/stream->seq ch))]
      (is= [val val] s mesg)))
  #_(let [s (decode-stream-headers (split-stream split-fn frame val) frame)
          v1 @(s/take! s)
          v2 (s/stream->seq (decode-stream s frame))]
      (let [s (convert-result (cons v1 v2))]
        (is= [val val] s))))

(defn test-simple-roundtrip [f val]
  (let [bytes (encode f val)
        val* (convert-char-sequences (decode f bytes))]
    (is (= val val*))))

(defn test-roundtrip [fr val]
  (let [codec (compile-frame fr)
        bytes (encode codec val)
        val* (convert-char-sequences (decode codec bytes))
        bytes (encode-all codec [val val])
        result (decode-all codec bytes)
        contiguous-result (decode-all codec (contiguous bytes))
        split-result (->> bytes to-buf-seq dup-bytes (partition-bytes 1) (decode-all codec))
        lazy-result (lazy-decode-all codec bytes)
        lazy-contiguous-result (lazy-decode-all codec bytes)
        lazy-split-result (->> bytes to-buf-seq dup-bytes (partition-bytes 1) (lazy-decode-all codec))]
    (is= val val* "char seq conversion failed")
    (test-stream-roundtrip #(partition-bytes 1 %) codec val "roundtrip partition-bytes 1 failure")
    (is= [val val] result "basic codec failed")
    (is= [val val] contiguous-result "contiguous failed")
    (is= [val val] split-result "split failed")
    (is= [val val] lazy-result "lazy failed")
    (is= [val val] lazy-split-result "lazy split failed")
    (is= [val val] lazy-contiguous-result "lazy contiguous failed")
    (doseq [i (range 1 (byte-count bytes))]
      (is= [val val] (decode-all codec (apply concat (split-bytes i bytes))))
      (test-stream-roundtrip #(split-bytes i %) codec val (str "roundtrip split " i " failure")))))

(defn test-full-roundtrip [f buf val]
  (is= val (decode f (-> buf to-buf-seq dup-bytes)))
  (is (= buf (write-bytes f nil val))))

;;;

(deftest test-lists
  (test-roundtrip
    [:float32-be nil-frame :float32-le]
    [1 nil 2])
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

(deftest test-multi-delimited-header
  (let [h->b (fn [head]
               (case head
                 "CMD" (compile-frame ["CMD" (string :utf-8 :delimiters ["\r\n"])])
                 "TERM" (compile-frame ["TERM"])))
        b->h (fn [body] (first body))
        cmd->delim (fn [cmd] (if (= cmd "TERM") ["\r\n"] [" "]))
        codec (compile-frame (header (string :utf-8 :delimiters [" " "\r\n"] :value->delimiter cmd->delim)
                                     h->b b->h))
        cmd (encode codec ["CMD" "TOKEN"])
        term (encode codec ["TERM"])]
    (is (= (buf->string cmd) "CMD TOKEN\r\n"))
    (is (= (buf->string term) "TERM\r\n"))))

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
  #_#_
  (test-roundtrip
    (string :utf-8)
    "abcd")
  (test-roundtrip
    (repeated (string :utf-8 :delimiters ["\0"]))
    ["abc" "def"])
  (test-roundtrip
    [:a (string :utf-8 :delimiters ["xy" "xyz"])]
    [:a "abc"])
  #_#_#_#_
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

(defmacro is-encoded [vec codec data]
  `(is (= ~vec (into [] (.array ^ByteBuffer (first (encode ~codec ~data)))))))

(deftest test-int24s
  (testing "numbers"
    (test-roundtrip
      [:int24 :int24-le :int24-be :uint24 :uint24-le :uint24-be]
      (repeat 6 100)))
  (testing "numbers greater than int16 range"
    (test-roundtrip
      [:int24 :int24-le :int24-be :uint24 :uint24-le :uint24-be]
      (repeat 6 32779)))
  (testing "edge cases"
    (test-roundtrip
      [:int24 :int24-le :int24-be :uint24 :uint24-le :uint24-be]
      [8388607 8388607 8388607 16777215 16777215 16777215])
    (test-roundtrip
      [:int24 :int24-le :int24-be :uint24 :uint24-le :uint24-be]
      [-8388608 -8388608 -8388608 0 0 0]))
  (testing "checking results against netty codec output"
    (is-encoded [-20 -5 -1] [:int24-le] [-1044])
    (is-encoded [-1 -5 -20] [:int24-be] [-1044])
    (is-encoded [-1 -1 -1] [:int24-le] [-1])
    (is-encoded [-1 -1 -1] [:int24-be] [-1])

    (is-encoded [11 -128 0] [:int24-le] [32779])
    (is-encoded [0 -128 11] [:int24-be] [32779])
    (is-encoded [11 -128 0] [:uint24-le] [32779])
    (is-encoded [0 -128 11] [:uint24-be] [32779])

    (is-encoded [-1 -1 63] [:uint24-le] [4194303])
    (is-encoded [63 -1 -1] [:uint24-be] [4194303])

    ;; max value for uint24
    (is-encoded [-1 -1 -1] [:uint24-le] [16777215])
    (is-encoded [-1 -1 -1] [:uint24-be] [16777215])

    ;; max value for int24
    (is-encoded [-1 -1 127] [:int24-le] [8388607])
    (is-encoded [127 -1 -1] [:int24-be] [8388607])

    (is-encoded [0 0 -128] [:int24-le] [-8388608])
    (is-encoded [-128 0 0] [:int24-be] [-8388608])))

;;Confirmed an issue. There are two problems going on, I think.
;;
;;One is that, on close, the `connect-via` callback `f` inside `decode-stream` is called one last time with an empty vec to force out any remaining bytes. (Tho it's not clear why any of the remaining bytes wouldn't have been consumed, if there were enough to fill a frame, since `decode-byte-sequence` is greedy. Maybe it has some undocumented purpose, like a different value of `complete?`, or flushing out bytes held by a codec.) This would result in an extra nil at the end of the stream, or in the case of the `string` frame here, an empty string, neither of which are correct.
;;
;;The other problem is the race between the multiple streams used internally to build `decode-stream` when closing. The callback function takes long enough that sometimes the downstream is closed before the callback writes a final value to it. Based on the docs, it seems like we could wait on the deferred returned by `connect-via` before closing, but that doesn't work because Manifold has inconsistent upstream connections (e.g., see clj-commons/manifold#128), so closing the dst doesn't close the upstream Callback. This might be rectifiable with WeakReferences, but if so, it may be better to add a WeakRef universally to the Downstream objects. Then we could fix issues like the previous one and clj-commons/manifold#85. Zach seemed to think it would be tricky, but the lack of it is causing several bugs.
;;
;;Need to investigate further.

(deftest test-closing-decode-stream
  (testing "closing the input stream doesn't lose data"
    (let [num-nums 5]
      (dotimes [i 100]
        (testing (str "- test " i)
          (let [str-frame (string "utf-8")
                in (s/stream 0 (map #(encode str-frame %)))
                out (decode-stream in str-frame)
                out-closed (d/deferred)
                _ (s/on-closed out #(d/success! out-closed true))
                test-future (future
                              (dotimes [n num-nums]
                                @(s/put! in (str n)))       ; do not assert here, too slow!
                              (s/close! in))]
            (is (= (map str (range num-nums))
                   (repeatedly num-nums #(deref (s/take! out ::default)))))
            @test-future
            (is (= nil @(s/try-take! out 1000)))
            (is (not= ::timeout (d/timeout! out-closed 5000 ::timeout)))))))))

#_
(deftest orig-test-decode-stream
  (testing "closing the decoded stream doesn't lose data"
    (dotimes [test-count 100]
      (let [str-frame (string "utf-8")
            in (s/stream 1 (map #(encode str-frame %)))
            out (decode-stream in str-frame)]
        (future
          (dotimes [n 10]
            @(s/put! in (str n)))
          (s/close! in))
        (is (= (map str (range 10))
               (repeatedly 10 #(deref (s/take! out)))))))))
