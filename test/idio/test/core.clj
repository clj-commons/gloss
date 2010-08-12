;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns idio.test.core
  (:use [idio] [clojure.contrib.combinatorics] :reload-all)
  (:use [clojure.test]))

(def types [:byte :short :int :long :float :double])

(defn equivalent? [a b]
  (every? identity (map == a b)))

(deftest mixed-types
  (doseq [sig (permutations types)]
    (let [data (range (count sig))]
      (is (equivalent? data (first (from-bytes (to-bytes data sig) sig)))))))

(def structures
  {[:int [:int :int] :int] [1 [2 3] 4]
   '(:int {:a :int :b :int} :int) '(1 {:a 2 :b 3} 4)
   {:a [:int :int :int :int]} {:a [1 2 3 4]}})

(deftest mixed-structures
  (let [buf (to-bytes [1 2 3 4] :int)]
    (doseq [[structure result] structures]
      (is (= result (first (from-bytes buf structure)))))))
