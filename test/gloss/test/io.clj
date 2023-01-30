;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns gloss.test.io
  (:use
   [clojure test])
  (:require
   [gloss.core :as gloss]
   [gloss.io :as io]
   [manifold.stream :as s]))

(deftest decode-stream
  (testing "closing the decoded stream doesn't lose data"
    (dotimes [test-count 100]
      (let [str-frame (gloss/string "utf-8")
            in (s/stream 0 (map #(io/encode str-frame %)))
            out (io/decode-stream in str-frame)]
        (future
          (dotimes [n 10]
            @(s/put! in (str n)))
          (s/close! in))
        (is (= (map str (range 10))
               (repeatedly 10 #(deref (s/take! out)))))))))
