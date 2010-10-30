;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns gloss.string
  (:use
    [gloss.core])
  (:import
    [java.nio
     ByteBuffer
     CharBuffer]
    [java.nio.charset
     CharsetDecoder
     Charset
     CoderResult]))

(defprotocol CharBufferSeq
  (char-buffer-seq [s])
  (remainder-chars [s]))

(defn- create-char-buf
  [^CharsetDecoder decoder ^ByteBuffer byte-buf]
  (CharBuffer/allocate (int (Math/ceil (/ (.remaining byte-buf) (.averageCharsPerByte decoder))))))

(defn- take-string-from-buf
  "Takes a single ByteBuffer, and turns it into a sequence of CharBuffers."
  [^CharsetDecoder decoder, ^ByteBuffer byte-buf, ^CharBuffer char-buf, handle-overflow?]
  (let [byte-buf (.duplicate byte-buf)
	char-buf (.duplicate ^CharBuffer (or char-buf (create-char-buf decoder byte-buf)))]
    (loop [chars [char-buf]]
      (let [result (-> decoder (.decode byte-buf (last chars) false))]
	(if (and handle-overflow? (.isOverflow result))
	  (recur (conj chars (create-char-buf decoder byte-buf)))
	  (let [last-char ^CharBuffer (last chars)]
	    {:remainder-bytes (when (.hasRemaining byte-buf)
				(.slice byte-buf))
	     :remainder-chars (when (.hasRemaining last-char)
				(-> last-char .duplicate .slice))
	     :chars (if-not (.hasRemaining last-char)
		      (map #(.rewind ^CharBuffer %) chars)
		      (conj
			(vec (map #(.rewind ^CharBuffer %) (drop-last chars)))
			(let [pos (.position last-char)]
			  (-> last-char (.position 0) (.limit pos)))))}))))))

'(defn- take-finite-string-from-buf
  [^CharsetDecoder decoder, ])

(defn- take-char-from-buf-seq
  [^CharsetDecoder decoder buf-seq]
  (let [char-buf (CharBuffer/allocate 1)]
    (.reset decoder)
    (loop [byte-count 1]
      (.rewind char-buf)
      (let [bytes (take-contiguous-bytes byte-count buf-seq)]
	(if-not bytes
	  [nil buf-seq]
	  (let [chars (first (:chars (take-string-from-buf decoder bytes char-buf false)))]
	    (if (pos? (.length chars))
	      [(.rewind char-buf) (drop-bytes byte-count buf-seq)]
	      (recur (inc byte-count)))))))))

(declare wrap-string-)

(defn- create-wrapped-string [charset chars remainder-bytes remainder-chars]
  (reify
    CharBufferSeq
    (char-buffer-seq [_] chars)
    (remainder-chars [_] remainder-chars)
    BufferSeq
    (buffer-seq [_] )
    (remainder-buffers [_] remainder-bytes)
    (concat-bytes [_ b]
      (wrap-string-
	(apply concat (map to-buffer-seq [remainder-bytes b]))
	chars
	charset
	remainder-chars
	true))
    (toString [_] (apply str chars))))

(defn- wrap-string- [buf-seq chars charset remainder-chars handle-overflow?]
  (let [charset (if (string? charset) (Charset/forName charset) charset)
	decoder (.newDecoder charset)]
    (loop [bytes buf-seq, chars chars, remainder-chars remainder-chars]
      (if (empty? bytes)
	(create-wrapped-string charset chars nil remainder-chars)
	(let [result (take-string-from-buf decoder (first bytes) remainder-chars handle-overflow?)]
	  (if-let [remainder-bytes (:remainder-bytes result)]
	    (let [consumed-bytes (- (.remaining ^ByteBuffer (first bytes)) (.remaining ^ByteBuffer remainder-bytes))
		  [char bytes] (take-char-from-buf-seq decoder (drop-bytes consumed-bytes bytes))]
	      (if-not char
		(create-wrapped-string charset chars remainder-bytes (:remainder-chars result))
		(recur bytes (concat chars (:chars result) [char]) (:remainder-chars result))))
	    (recur (drop 1 bytes) (concat chars (:chars result)) (:remainder-chars result))))))))

(defn wrap-string
  ([buf-seq]
     (wrap-string buf-seq "utf-8"))
  ([buf-seq charset]
     (wrap-string- (to-buffer-seq buf-seq) nil charset nil true)))
