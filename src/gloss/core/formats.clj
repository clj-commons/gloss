;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^{:skip-wiki true}
  gloss.core.formats
  (:use
    [gloss.data.bytes.core])
  (:import
    [java.nio
     Buffer
     ByteBuffer
     CharBuffer]))

(def byte-array-class (class (byte-array [])))

(defn to-buf-seq
  "Converts the value to a sequence of ByteBuffers."
  [x]
  (when x
    (cond
      (= (class x) byte-array-class)
      (create-buf-seq (ByteBuffer/wrap x))

      :else
      (create-buf-seq x))))

(defn to-byte-buffer
  "Converts the value to a Bytebuffer."
  [x]
  (when x
    (cond
      (instance? Character x) (to-byte-buffer (str x))
      (string? x) (to-byte-buffer (.getBytes ^String x "utf-8"))
      (= (class x) byte-array-class) (ByteBuffer/wrap x)
      (instance? ByteBuffer x) x
      (sequential? x) (to-byte-buffer (byte-array (map #(byte (int %)) x)))
      (number? x) (to-byte-buffer (byte-array [(byte x)]))
      :else (throw (Exception. (str "Cannot convert to ByteBuffer: " x))))))

(defn to-char-buffer
  "Converts the value to a CharBuffer."
  [x]
  (when x
    (if (instance? CharBuffer x)
      x
      (CharBuffer/wrap ^CharSequence x))))

(defn string-to-byte-buffer
  ([s charset] 
   (->> s
     (map #(if (string? %) (.getBytes ^String % charset) %))
     (map to-byte-buffer)))
  ([s] (string-to-byte-buffer s "utf-8")))
