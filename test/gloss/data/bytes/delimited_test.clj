(ns gloss.data.bytes.delimited-test
  (:require
    [clojure.test :refer [deftest is testing]]
    [gloss.data.bytes.delimited :as delimited])
  (:import
    (java.nio ByteBuffer)))



(deftest match-loop
  (testing "one"
    (let [d1 (ByteBuffer/allocate 1)
          ml (delimited/match-loop
               [d1]
               false)]
      (is (= [true 1 1]
             (ml d1 true)))
      (is (= [false 0 0]
             (ml d1 false)))))
  (testing "many"
    (let [d1 (ByteBuffer/allocate 1)
          d2 (ByteBuffer/allocate 2)
          d3 (ByteBuffer/allocate 3)
          ml (delimited/match-loop
               [d3 d2 d1]
               false)]
      (is (= [true 1 1]
             (ml d1 true)))
      (is (= [true 2 2]
             (ml d2 true)))
      (is (= [true 3 3]
             (ml d3 true)))
      (is (= [false 0 0]
             (ml d1 false)))
      (is (= [false 0 0]
             (ml d2 false)))
      (is (= [false 0 0]
             (ml d3 false))))))
