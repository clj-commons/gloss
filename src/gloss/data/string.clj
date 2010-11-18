;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns gloss.data.string
  (:use
    [gloss.data bytes]
    [gloss.core protocols formats])
  (:import
    [java.nio
     Buffer
     ByteBuffer
     CharBuffer]
    [java.nio.charset
     CharsetDecoder
     CharsetEncoder
     Charset
     CoderResult]))

(defn- create-char-buf
  [^CharsetDecoder decoder buf-seq]
  (CharBuffer/allocate (int (Math/ceil (/ (buf-seq-count buf-seq) (.averageCharsPerByte decoder))))))

(defn- nth-char [char-buf-seq length idx]
  (if (neg? idx)
    (throw (IndexOutOfBoundsException. (str idx " is not a valid index.")))
    (loop [idx idx chars chars]
      (let [buf ^CharBuffer (first chars)]
	(cond
	  (nil? buf) (throw (IndexOutOfBoundsException. (str idx "is greater than length of " length)))
	  (> idx (.remaining buf)) (recur (- idx (.remaining buf)) (rest chars))
	  :else (.get buf (int idx)))))))

(defn- sub-sequence [buf-seq length start end]
  (if (or (neg? start) (<= length end))
    (throw (IndexOutOfBoundsException. (str "[" start ", " end ") is not a valid interval.")))
    (-> buf-seq (drop-from-char-bufs start) (take-from-char-bufs (- end start)))))

(defn- create-decoder [charset]
  (.newDecoder (Charset/forName charset)))

(defn- create-encoder [charset]
  (.newEncoder (Charset/forName charset)))

(defn create-char-buf-seq [chars]
  (let [length (apply + (map #(.remaining ^CharBuffer %) chars))]
    (reify
      
      CharSequence
      (charAt [this idx] (nth-char chars length idx))
      (length [_] length)
      (subSequence [_ start end] (sub-sequence chars length start end))
      (toString [_] (apply str chars))
      
      clojure.lang.Counted
      (count [_] length))))

(defn to-char-buffer [x]
  (if (instance? CharBuffer x)
    x
    (CharBuffer/wrap x)))

;;;

(defn- take-finite-string-from-buf-seq [^CharsetDecoder decoder ^CharBuffer char-buf buf-seq]
  (let [buf-seq (dup-buf-seq buf-seq)]
    (if-not (.hasRemaining char-buf)
      [char-buf buf-seq]
      (loop [bytes buf-seq]
	(if (empty? bytes)
	  [char-buf nil]
	  (let [first-buf ^ByteBuffer (first bytes)
		result (.decode decoder first-buf char-buf false)]
	    (cond
	      
	      (.isOverflow result)
	      [char-buf bytes]
	      
	      (and (.isUnderflow result) (pos? (.remaining first-buf)))
	      (if (= 1 (count bytes))
		[char-buf bytes]
		(recur
		  (cons
		    (take-contiguous-bytes (inc (buf-seq-count (take 1 bytes))) bytes)
		    (drop-bytes 1 (rest bytes)))))
	      
	      :else
	      (recur (rest bytes)))))))))

(defn finite-string-codec- [charset len decoder char-buf]
  (reify
    Reader
    (read-bytes [this buf-seq]
      (let [decoder (or decoder (create-decoder charset))
	    char-buf (or char-buf (CharBuffer/allocate len))
	    [^CharBuffer chars bytes] (take-finite-string-from-buf-seq decoder char-buf buf-seq)]
	(if-not (.hasRemaining chars)
	  [true (.rewind chars) bytes]
	  [false (finite-string-codec- charset len decoder char-buf) buf-seq])))
    Writer
    (sizeof [_]
      nil)
    (write-bytes [_ _ strs]
      (let [encoder (create-encoder charset)]
	(apply concat
	  (map #(.encode ^CharsetEncoder encoder (to-char-buffer %)) strs))))))

(defn finite-string-codec
  [charset len]
  (finite-string-codec- charset len nil nil))

;;;

(defn take-string-from-buf-seq [^CharsetDecoder decoder, buf-seq]
  (let [buf-seq (dup-buf-seq buf-seq)
	char-buf (create-char-buf decoder buf-seq)]
    (loop [chars [char-buf], bytes buf-seq]
      (if (empty? bytes)
	[(rewind-char-buf-seq chars) nil]
	(let [first-buf ^ByteBuffer (first bytes)
	      result (-> decoder (.decode first-buf (last chars) false))]
	  (cond

	    (.isOverflow result)
	    (recur (conj chars (create-char-buf decoder bytes)) bytes)

	    (and (.isUnderflow result) (pos? (.remaining first-buf)))
	    (if (= 1 (count bytes))
	      [(rewind-char-buf-seq chars) bytes]
	      (recur chars
		(cons
		  (take-contiguous-bytes (inc (buf-seq-count (take 1 bytes))) bytes)
		  (drop-bytes 1 (rest bytes)))))

	    :else
	    (recur chars (rest bytes))))))))

(defn string-codec [charset]
  (reify
    Reader
    (read-bytes [this buf-seq]
      (let [decoder (create-decoder charset)
	    [chars bytes] (take-string-from-buf-seq decoder buf-seq)]
	(if (zero? (buf-seq-count chars))
	  [false this buf-seq]
	  [true (create-char-buf-seq chars) bytes])))
    Writer
    (sizeof [_]
      nil)
    (write-bytes [_ _ s]
      [(.encode ^CharsetEncoder (create-encoder charset) (to-char-buffer s))])))
