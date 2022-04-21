;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^{:skip-wiki true}
  gloss.core.formats
  (:require
    [clj-commons.byte-streams :as bs]
    [gloss.data.bytes.core :refer :all])
  (:import
    [java.nio
     Buffer
     ByteBuffer
     CharBuffer]))

(def byte-array-class (class (byte-array [])))

(defn to-buf-seq
  "Converts the value to a sequence of ByteBuffers."
  [x]
  (if (or (nil? x) (and (sequential? x) (empty? x)))
    (create-buf-seq x)
    (create-buf-seq (bs/convert x (bs/seq-of ByteBuffer)))))

(defn to-byte-buffer
  "Converts the value to a Bytebuffer."
  [x]
  (when x
    (cond
      (sequential? x) (bs/to-byte-buffer (map to-byte-buffer x))
      (instance? Character x) (to-byte-buffer (str x))
      (number? x) (to-byte-buffer (byte-array [(byte x)]))
      :else (bs/to-byte-buffer x))))

(defn to-char-buffer
  "Converts the value to a CharBuffer."
  [x]
  (when x
    (if (instance? CharBuffer x)
      x
      (CharBuffer/wrap ^CharSequence (bs/convert x CharSequence)))))

(defn string-to-byte-buffer
  ([s ^String charset]
   (->> s
     (map #(if (string? %) (.getBytes ^String % charset) %))
     (map to-byte-buffer)))
  ([s]
     (string-to-byte-buffer s "utf-8")))
