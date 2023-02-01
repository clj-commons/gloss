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
