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
     Buffer
     ByteBuffer
     CharBuffer]
    [java.nio.charset
     CharsetDecoder
     Charset
     CoderResult]))

(defprotocol CharBufferSeq
  (char-buffer-seq [s]))

(defn- take-finite-string-from-buf )

(defn- create-char-buf
  [^CharsetDecoder decoder buf-seq]
  (CharBuffer/allocate (int (Math/ceil (/ (buffer-seq-count buf-seq) (.averageCharsPerByte decoder))))))

(defn rewind-buf-seq [buf-seq]
  (concat
    (map #(.rewind ^Buffer %) (drop-last buf-seq))
    (let [last-buf (last buf-seq)
	  last-pos (.position last-buf)]
      [(-> last-buf (.position 0) (.limit last-pos) .slice)])))

(defn nth-char [char-buf-seq length idx]
  (if (neg? idx)
    (throw (IndexOutOfBoundsException. (str idx " is not a valid index.")))
    (loop [idx idx chars chars]
      (let [buf ^CharBuffer (first chars)]
	(cond
	  (nil? buf) (throw (IndexOutOfBoundsException. (str idx "is greater than length of " length)))
	  (> idx (.remaining buf)) (recur (- idx (.remaining buf)) (rest chars))
	  :else (.get buf idx))))))

(defn sub-sequence [buf-seq length start end]
  (if (or (neg? start) (<= length end))
    (throw (IndexOutOfBoundsException. (str "[" start ", " end ") is not a valid interval.")))
    (-> buf-seq (drop-from-bufs start) (take-from-bufs (- end start)))))

;;;

(defn take-finite-string-from-buf-seq [^CharsetDecoder decoder ^CharBuffer char-buf buf-seq]
  (let [buf-seq (dup-buffer-seq buf-seq)]
    (if-not (.hasRemaining char-buf)
      [char-buf buf-seq]
      (loop [bytes buf-seq]
	(if (empty? bytes)
	  [char-buf nil]
	  (let [first-buf (first bytes)
		result (.decode decoder first-buf char-buf false)]
	    (cond
	      
	      (.isOverflow result)
	      [char-buf bytes]
	      
	      (and (.isUnderflow result) (pos? (.remaining first-buf)))
	      (if (= 1 (count bytes))
		[char-buf bytes]
		(recur
		  (cons
		    (take-contiguous-bytes (inc (buffer-seq-count (take 1 bytes))) bytes)
		    (drop-bytes 1 (rest bytes)))))
	      
	      :else
	      (recur (rest bytes)))))))))

(defn take-string-from-buf-seq [^CharsetDecoder decoder, buf-seq]
  (let [buf-seq (dup-buffer-seq buf-seq)
	char-buf (create-char-buf decoder buf-seq)]
    (loop [chars [char-buf], bytes buf-seq]
      (if (empty? bytes)
	[(rewind-buf-seq chars) nil]
	(let [first-buf (first bytes)
	      result (-> decoder (.decode first-buf (last chars) false))]
	  (cond

	    (.isOverflow result)
	    (recur (conj chars (create-char-buf decoder bytes)) bytes)

	    (and (.isUnderflow result) (pos? (.remaining first-buf)))
	    (if (= 1 (count bytes))
	      [(rewind-buf-seq chars) bytes]
	      (recur chars
		(cons
		  (take-contiguous-bytes (inc (buffer-seq-count (take 1 bytes))) bytes)
		  (drop-bytes 1 (rest bytes)))))

	    :else
	    (recur chars (rest bytes))))))))

;;;

(defn- create-wrapped-string [^CharsetDecoder decoder chars buf-seq]
  (let [length (apply + (map #(.remaining ^CharBuffer %) chars))]
    (reify

      CharBufferSeq
      (char-buffer-seq [_] chars)

      BufferSeq
      (buffer-seq [_]
	(let [encoder (-> decoder .charset .newEncoder)]
	  (concat (map #(.encode encoder %) chars) buf-seq)))
      (remainder-bytes [_] remainder-bytes)
      (concat-bytes [_ b]
	(let [b (to-buffer-seq b)
	      [c b] (take-string-from-buf-seq decoder (concat buf-seq b))]
	  (create-wrapped-string decoder (concat chars c) b)))

      CharSequence
      (charAt [this idx] (nth-char chars length idx))
      (length [_] length)
      (subSequence [_ start end] (sub-sequence chars length start end))
      (toString [_] (apply str chars))

      clojure.lang.Counted
      (count [_] length))))

(defn wrap-string
  ([buf-seq]
     (wrap-string buf-seq "utf-8"))
  ([buf-seq charset]
     (let [decoder (.newDecoder (Charset/forName charset))
	   buf-seq (to-buffer-seq buf-seq)
	   [c b] (take-string-from-buf-seq decoder buf-seq)]
       (create-wrapped-string decoder nil (to-buffer-seq buf-seq)))))

;;;

(defn create-string-sequence [^CharsetDecoder decoder substring-length ^CharBuffer char-buf buf-seq]
  (if (and (nil? char-buf) (zero? (buffer-seq-count buf-seq)))
    nil
    (let [char-buf ^CharBuffer (or char-buf (CharBuffer/allocate substring-length))
	  [char-buf buf-seq] (take-finite-string-from-buf-seq decoder char-buf buf-seq)
	  complete? (not (.hasRemaining char-buf))
	  element (when complete? (-> char-buf .duplicate .rewind))]
      (reify

        BufferSeq
	(buffer-seq [_]
	  (let [encoder (-> decoder .charset .newEncoder)]
	    (concat
	      [(.encode encoder
		 (or
		   element
		   (-> char-buf .duplicate (.limit (.position char-buf)) (.position 0))))]
	      buf-seq)))
	(remainder-bytes [_]
	  buf-seq)
	(concat-bytes [_ b]
	  (let [b (to-buffer-seq b)]
	    (create-string-sequence
	      decoder
	      substring-length
	      char-buf
	      (concat buf-seq b))))
	
	clojure.lang.ISeq
	(first [_]
	  element)
	(next [this]
	  (when complete?
	    (create-string-sequence
	      decoder
	      substring-length
	      nil
	      buf-seq)))
	(more [this]
	  (when-let [n (next this)]
	    n
	    []))
	(cons [s this]
	  (cons s (seq this)))
	(seq [this]
	  (when complete?
	    (cons (first this) (next this))))))))

(defn wrap-string-sequence
  ([buf-seq substring-length]
     (wrap-string-sequence buf-seq substring-length "utf-8"))
  ([buf-seq substring-length charset]
     (let [decoder (.newDecoder (Charset/forName charset))
	   buf-seq (to-buffer-seq buf-seq)]
       (create-string-sequence decoder substring-length nil buf-seq))))


