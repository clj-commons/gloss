;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns gloss.data.string.core
  (:use
    [gloss.data bytes])
  (:import
    [java.nio
     Buffer
     CharBuffer]
    [java.nio.charset
     Charset
     CharsetDecoder
     CharsetEncoder]))

(defn rewind-chars [buf-seq]
  (concat
    (map #(.rewind ^CharBuffer %) (drop-last buf-seq))
    (let [last-buf ^CharBuffer (last buf-seq)
	  last-pos (.position last-buf)]
      [(-> last-buf (.position 0) ^CharBuffer (.limit last-pos) .slice)])))

(defn drop-chars
  [n buf-seq]
  (cond
    (not (pos? n))
    buf-seq

    (>= n (byte-count buf-seq))
    nil

    :else
    (loop [remaining n, s buf-seq]
      (when-not (empty? s)
	(let [buf ^CharBuffer (first s)]
	  (cond
	    (= remaining (.remaining buf))
	    (rest s)

	    (< remaining (.remaining buf))
	    (cons
	      (-> buf .duplicate ^CharBuffer (.position (+ remaining (.position buf))) .slice)
	      (rest s))

	    :else
	    (recur (- remaining (.remaining buf)) (rest s))))))))

(defn take-chars
  [n buf-seq]
  (cond
    (not (pos? n))
    nil

    (>= n (byte-count buf-seq))
    buf-seq

    :else
    (when-let [first-buf ^CharBuffer (first buf-seq)]
      (if (> (.remaining first-buf) n)
	[(-> first-buf .duplicate ^CharBuffer (.limit (+ (.position first-buf) n)) .slice)]
	(when (<= n (byte-count buf-seq))
	  (loop [remaining n, s buf-seq, accumulator []]
	    (if (pos? remaining)
	      (let [buf ^CharBuffer (first s)]
		(if (>= remaining (.remaining buf))
		  (recur (- remaining (.remaining buf)) (rest s) (conj accumulator buf))
		  (conj accumulator (-> buf .duplicate ^CharBuffer (.limit (+ (.position buf) remaining)) .slice))))
	      accumulator)))))))


(defn create-char-buf
  [^CharsetDecoder decoder buf-seq]
  (CharBuffer/allocate (int (Math/ceil (/ (byte-count buf-seq) (.averageCharsPerByte decoder))))))

(defn nth-char [char-buf-seq length idx]
  (if (neg? idx)
    (throw (IndexOutOfBoundsException. (str idx " is not a valid index.")))
    (loop [idx idx chars char-buf-seq]
      (let [buf ^CharBuffer (first chars)]
	(cond
	  (nil? buf) (throw (IndexOutOfBoundsException. (str idx "is greater than length of " length)))
	  (> idx (.remaining buf)) (recur (- idx (.remaining buf)) (rest chars))
	  :else (.get buf (int idx)))))))

(defn sub-sequence [char-buf-seq length start end]
  (if (or (neg? start) (<= length end))
    (throw (IndexOutOfBoundsException. (str "[" start ", " end ") is not a valid interval.")))
    (-> char-buf-seq (drop-chars start) (take-chars (- end start)))))

(defn create-decoder [charset]
  (.newDecoder (Charset/forName charset)))

(defn create-encoder [charset]
  (.newEncoder (Charset/forName charset)))

(defn create-char-sequence [char-buf-seq]
  (let [length (apply + (map #(.remaining ^CharBuffer %) char-buf-seq))]
    ^{:type ::char-sequence}
    (reify
      
      CharSequence
      (charAt [this idx] (nth-char char-buf-seq length idx))
      (length [_] length)
      (subSequence [_ start end] (sub-sequence char-buf-seq length start end))
      (toString [_] (apply str char-buf-seq))
      
      clojure.lang.Counted
      (count [_] length))))

(defmethod print-method ::char-sequence [char-seq writer]
  (print-method (str char-seq) writer))
