;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns idio.test.core
  (:use [idio.core])
  (:use [clojure.test])
  (:import [java.nio ByteBuffer]))

(defn wrap-seq [seq]
  (ByteBuffer/wrap (byte-array (map byte seq))))

(deftest test-drop-bytes
  (let [bufs (map wrap-seq (partition 5 (range 100)))]
    (dotimes [i 100]
      (is (= (drop i (range 100)) (mapcat byte-seq (drop-bytes i bufs)))))))

(deftest test-take-bytes
  (let [bufs (map wrap-seq (partition 5 (range 100)))]
    (dotimes [i 100]
      (is (= (take i (range 100)) (mapcat byte-seq (take-bytes i bufs)))))))

(deftest test-take-contiguous-bytes
  (let [bufs (map wrap-seq (partition 5 (range 100)))]
    (dotimes [i 100]
      (is (= (take i (range 100)) (byte-seq (take-contiguous-bytes i bufs)))))))
