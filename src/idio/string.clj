;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns idio.string
  (:use
    [idio.core])
  (:import
    [java.nio
     ByteBuffer
     CharBuffer]
    [java.nio.charset
     CharsetDecoder
     Charset
     CoderResult]))

(defn- create-char-buf
  [^CharsetDecoder decoder ^ByteBuffer byte-buf]
  (CharBuffer/allocate (int (Math/ceil (/ (.remaining byte-buf) (.averageCharsPerByte decoder))))))

(defn- take-string-from-buf
  [^CharsetDecoder decoder, ^ByteBuffer byte-buf, ^CharBuffer char-buf]
  (loop [chars [(if char-buf char-buf (create-char-buf decoder byte-buf))]]
    (let [result (-> decoder (.decode byte-buf (last chars) false))]
      (when (.isError result)
	(.throwException result))
      (if (.isOverflow result)
	(recur (conj chars (create-char-buf decoder byte-buf)))
	chars))))

(defn take-string
  ([s]
     (take-string s "UTF-8"))
  ([s charset]
     (take-string s charset nil))
  ([s charset buf]
     (let [charset (Charset/forName charset)
	   decoder (.newDecoder charset)]
       )))
