(ns gloss.data.bytes.delimited-test
  (:require
    [clojure.test :refer [deftest is testing]]
    [gloss.data.bytes.delimited :as delimited]
    [malli.core :as m]
    [malli.generator :as mg])
  (:import
    (java.nio ByteBuffer)))



(deftest match-loop
  (testing "zero"
    (let [d0 (ByteBuffer/allocate 0)
          d1 (ByteBuffer/allocate 1)
          ml (delimited/match-loop
               [d0]
               false)
          ml2 (delimited/match-loop
                [d1]
                false)]
      (is (= [false 0 0]
             (ml d0 true)))
      (is (= [false 0 0]
             (ml d0 false)))
      (is (= [true 0 0]
             (ml d1 true)))
      (is (= [true 0 0]
             (ml d1 false)))
      (is (= [false 0 0]
             (ml2 d0 true)))
      (is (= [false 0 0]
             (ml2 d0 false)))
      (is (= [true 1 1]
             (ml2 d1 true)))
      (is (= [false 0 0]
             (ml2 d1 false)))))
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
             (ml d3 false)))))
  (testing "no match"
    (let [d1 (ByteBuffer/allocate 4)
          m1 (delimited/match-loop
               [
                (ByteBuffer/allocate 1)]
               false)
          m2 (delimited/match-loop
               [
                (ByteBuffer/allocate 1)]
               true)
          m3 (delimited/match-loop
               [(ByteBuffer/allocate 0)]
               false)
          m4 (delimited/match-loop
               [(ByteBuffer/allocate 0)]
               true)]
      (.put d1 (int 0) (byte 1))
      (.put d1 (int 1) (byte 1))
      (.put d1 (int 2) (byte 1))
      (.put d1 (int 3) (byte 1))
      (is (= [false 0 0]
             (m1 d1 true)))
      (is (= [false 0 0]
             (m1 d1 false)))
      (is (= [false 0 0]
             (m2 d1 true)))
      (is (= [false 0 0]
             (m2 d1 false)))
      (is (= [false 0 0]
             (m3 d1 true)))
      (is (= [false 0 0]
             (m3 d1 false)))
      (is (= [false 0 0]
             (m4 d1 true)))
      (is (= [false 0 0]
             (m4 d1 false))))))


(defn check-match-loop
  [{:keys [delimiters
           buffer
           strip-delimiters?
           last-and-complete?]}]
  (let [delimiters (mapv
                     (fn [delimiter]
                       (let [bb (ByteBuffer/allocate (count delimiter))]
                         (doall
                           (map-indexed
                             (fn [i v]
                               (.put bb (int i) (byte v)))
                             delimiter))
                         bb))
                     delimiters)
        ml (delimited/match-loop
             delimiters
             strip-delimiters?)
        buf (ByteBuffer/allocate (count buffer))]
    (doall
      (map-indexed
        (fn [i v]
          (.put buf (int i) (byte v)))
        buffer))
    (try
      (ml buf last-and-complete?)
      (catch Throwable t
        (println "error" t)
        t))))


(def byte-int
  [:int {:min -128 :max 127}])

(def binary-int
  [:int {:min 0 :max 1}])

(defn match-loop-schema [int-type]
  (m/schema
    [:map
     [:delimiters [:sequential
                   {:min 1}
                   [:sequential int-type]]]
     [:buffer [:sequential int-type]]
     [:strip-delimiters? boolean?]
     [:last-and-complete? boolean?]]))

(defn match-loop-fn-schema [int-type]
  (m/schema
    [:=>
     [:cat (match-loop-schema int-type)]
     [:tuple
      boolean?
      integer?
      integer?]]
    {::m/function-checker mg/function-checker}))

(deftest match-loop-malli
  (testing "binary"
    (let [schema (match-loop-fn-schema binary-int)]
      (is (true?
            (or
              (m/validate schema check-match-loop)
              (m/explain schema check-match-loop))))))
  (testing "bytes"
    (let [schema (match-loop-fn-schema byte-int)]
      (is (true?
            (or
              (m/validate schema check-match-loop)
              (m/explain schema check-match-loop)))))))
