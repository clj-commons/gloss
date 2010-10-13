;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns gloss.string
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

(defprotocol CharBufferSeq
  (remainder-chars [s]))

(defn- create-char-buf
  [^CharsetDecoder decoder ^ByteBuffer byte-buf]
  (CharBuffer/allocate (int (Math/ceil (/ (.remaining byte-buf) (.averageCharsPerByte decoder))))))

(defn- take-string-from-buf
  [^CharsetDecoder decoder, ^ByteBuffer byte-buf, ^CharBuffer char-buf]
  (let [byte-buf (.duplicate byte-buf)]
    (loop [chars [(if char-buf char-buf (create-char-buf decoder byte-buf))]]
      (let [result (-> decoder (.decode byte-buf (last chars) false))]
	(when (.isError result)
	  (.throwException result))
	(if (.isOverflow result)
	  (recur (conj chars (create-char-buf decoder byte-buf)))
	  (let [last-char ^CharBuffer (last chars)]
	    {:remainder-bytes (when (.hasRemaining byte-buf) (.slice byte-buf))
	     :remainder-chars (when (.hasRemaining last-char) (-> last-char .duplicate .slice))
	     :chars (if-not (.hasRemaining last-char)
		      (map #(.rewind ^CharBuffer %) chars)
		      (concat
			(map #(.rewind ^CharBuffer %) (drop-last chars))
			(let [pos (.position last-char)]
			  [(-> last-char (.position 0) (.limit pos))])))}))))))

(defn take-string
  ([buf-seq]
     (take-string buf-seq "UTF-8"))
  ([buf-seq charset]
     (take-string buf-seq charset nil))
  ([buf-seq charset buf]
     (let [charset (Charset/forName charset)
	   decoder (.newDecoder charset)]
       )))
