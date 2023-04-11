(ns gloss.data.bytes.core-test
  (:require [clojure.test :refer :all]
            [gloss.core :as gloss]
            [gloss.data.bytes :as gdb]
            [gloss.io :as gl.io])
  (:import (gloss.data.bytes.core SingleBufferSequence)
           (java.nio ByteBuffer)))

(def test-string "\"abc\"")

(defn test-bb
  "Encodes string \"abc\""
  ^ByteBuffer
  []
  (doto (ByteBuffer/allocate 40)
        (.put (byte 97))
        (.put (byte 97))
        (.put (byte 97))
        (.put (byte 97))
        (.putInt (int 5))
        (.put (byte 34))
        (.put (byte 97))
        (.put (byte 98))
        (.put (byte 99))
        (.put (byte 34))
        (.flip)
        (.position 4)))

;; From https://github.com/clj-commons/gloss/issues/63
(deftest handle-nonzero-positions
  (let [bb (test-bb)
        curr-pos (.position bb)
        remaining (.remaining bb)
        num-to-drop (rand-int 4)
        expected-size (- remaining num-to-drop)
        bb-bufseq (SingleBufferSequence. bb (.remaining bb))
        codec (gloss/finite-frame (gloss/prefix :int32)
                                  (gloss/string :utf-8))]
    (is (= test-string
           (gl.io/decode codec bb true)))

    (is (= expected-size
           (gdb/byte-count (gdb/drop-bytes bb-bufseq num-to-drop))))))

