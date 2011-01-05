;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^{:skip-wiki true}
  gloss.data.string.codecs
  (:use
    [gloss.core formats protocols]
    [gloss.data.bytes]
    [gloss.data.string.core])
  (:import
    [java.nio
     Buffer
     ByteBuffer
     CharBuffer]
    [java.nio.charset
     Charset
     CharsetDecoder
     CharsetEncoder]))

(defn take-string-from-buf-seq [^CharsetDecoder decoder, buf-seq]
  (let [buf-seq (dup-bytes buf-seq)
	char-buf (create-char-buf decoder buf-seq)]
    (loop [chars [char-buf], bytes buf-seq]
      (if (empty? bytes)
	[(rewind-chars chars) nil]
	(let [first-buf ^ByteBuffer (first bytes)
	      result (-> decoder (.decode first-buf (last chars) false))]
	  (cond

	    (.isOverflow result)
	    (recur (conj chars (create-char-buf decoder bytes)) bytes)

	    (and (.isUnderflow result) (pos? (.remaining first-buf)))
	    (if (= 1 (count bytes))
	      [(rewind-chars chars) bytes]
	      (recur chars
		(cons
		  (take-contiguous-bytes bytes (inc (.remaining ^ByteBuffer (first bytes))))
		  (drop-bytes (rest bytes) 1))))

	    :else
	    (recur chars (rest bytes))))))))

(defn string-codec [charset]
  (reify
    Reader
    (read-bytes [this buf-seq]
      (let [decoder (create-decoder charset)]
	(if (and (single-buffer? buf-seq) complete?)
	  [true (.decode decoder (first buf-seq)) nil]
	  (let [[chars bytes] (take-string-from-buf-seq decoder buf-seq)]
	    (if (empty? chars)
	      [false this buf-seq]
	      [true (create-char-sequence chars) bytes])))))
    Writer
    (sizeof [_]
      nil)
    (write-bytes [_ _ s]
      (when-not (instance? CharSequence s)
	(throw (Exception. (str "Expected a CharSequence, but got " s " " (class s)))))
      (cond
	(empty? s)
	nil
	
	(string? s)
	[(ByteBuffer/wrap (.getBytes ^String s (name charset)))]

	:else
	[(.encode ^CharsetEncoder (create-encoder charset) (to-char-buffer s))]))))
