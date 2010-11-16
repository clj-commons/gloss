;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns gloss.core.formats
  (:import
    [java.nio Buffer ByteBuffer]))

(def byte-array-class (class (byte-array [])))

(defn to-buf-seq [x]
  (when x
    (cond
      (and (sequential? x) (or (empty? x) (instance? ByteBuffer (first x)))) x
      (= (class x) byte-array-class) [(ByteBuffer/wrap x)]
      (instance? ByteBuffer x) [x]
      :else (throw (Exception. (str "Cannot convert to buf-seq: " x))))))

(defn to-byte-buffer [x]
  (cond
    (instance? Character x) (to-byte-buffer (str x))
    (string? x) (to-byte-buffer (.getBytes x "utf-8"))
    (= (class x) byte-array-class) (ByteBuffer/wrap x)
    (instance? ByteBuffer x) x
    (sequential? x) (to-byte-buffer (byte-array (map byte x)))
    (number? x) (to-byte-buffer (byte-array [(byte x)]))
    :else (throw (Exception. (str "Cannot convert to ByteBuffer: " x)))))
