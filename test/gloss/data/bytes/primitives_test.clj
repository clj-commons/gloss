(ns gloss.data.bytes.primitives-test
  (:require [clojure.test :refer :all]
            [gloss.core :as gloss]
            [gloss.core.protocols :as gloss.proto]
            [gloss.data.bytes :as gdb]
            [gloss.data.primitives :as prim]
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
        (.put (byte 97))
        (.put (byte 97))
        (.putInt (int 5))
        (.put (byte 34))
        (.put (byte 97))
        (.put (byte 98))
        (.put (byte 99))
        (.put (byte 34))
        (.flip)
        (.position 6)))

;; From https://github.com/clj-commons/gloss/issues/63
(deftest handle-nonzero-positions
  (let [bb (test-bb)
        remaining (.remaining bb)
        int-size 4
        expected-size (- remaining int-size)
        bb-bufseq (SingleBufferSequence. bb remaining)
        codec (:int32 prim/primitive-codecs)
        [_ _ remainder] (gloss.proto/read-bytes codec bb-bufseq)]

    (is (= expected-size
           (gdb/byte-count remainder)))
    (is (= expected-size
           (.remaining ^ByteBuffer (first remainder))))))

