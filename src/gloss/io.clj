;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns gloss.io
  (:use
    [gloss.core codecs structure protocols]
    [potemkin]
    [lamina core])
  (:require
    [gloss.core.formats :as formats]
    [gloss.data.bytes :as bytes])
  (:import
    [java.nio.channels
     Channels]
    [java.nio
     ByteBuffer]
    [java.io
     InputStream]))

;;;

(import-fn #'formats/to-byte-buffer)
(import-fn #'formats/to-buf-seq)

(defn contiguous
  "Takes a sequence of ByteBuffers and returns a single contiguous ByteBuffer."
  [buf-seq]
  (when-let [buf-seq (to-buf-seq buf-seq)]
    (bytes/take-contiguous-bytes buf-seq (bytes/byte-count buf-seq))))

;;;

(defn encode
  "Turns a frame value into a sequence of ByteBuffers."
  [codec val]
  (when val
    (write-bytes codec nil val)))

(defn encode-to-buffer
  "Encodes a sequence of values, and writes them to a ByteBuffer."
  [codec buf vals]
  (assert (sizeof codec))
  (doseq [v vals]
    (write-bytes codec buf v)))

(defn encode-all
  "Turns a sequence of frame values into a sequence of ByteBuffers."
  [codec vals]
  (if-let [size (sizeof codec)]
    (let [buf (ByteBuffer/allocate (* size (count vals)))]
      (encode-to-buffer codec buf vals)
      [(.rewind buf)])
    (apply concat
      (map #(write-bytes codec nil %) vals))))

(defn encode-to-stream
  "Encodes a sequence of values, and writes them to an OutputStream."
  [codec output-stream vals]
  (let [channel (Channels/newChannel output-stream)]
    (doseq [buf (encode-all codec vals)]
      (.write channel ^ByteBuffer buf))))

;;;

(defn decode
  "Turns bytes into a single frame value.  If there are too few or too many bytes
   for the frame, an exception is thrown."
  [codec bytes]
  (let [buf-seq (bytes/dup-bytes (to-buf-seq bytes))
	[success val remainder] (read-bytes codec buf-seq true)]
    (when-not success
      (throw (Exception. "Insufficient bytes to decode frame.")))
    (when-not (empty? remainder)
      (throw (Exception. "Bytes left over after decoding frame.")))
    val))

(defn decode-all
  "Turns bytes into a sequence of frame values.  If there are bytes left over at the end
   of the sequence, an exception is thrown."
  [codec bytes]
  (let [buf-seq (bytes/dup-bytes (to-buf-seq bytes))]
    (loop [buf-seq buf-seq, vals []]
      (if (empty? buf-seq)
	vals
	(let [[success val remainder] (read-bytes codec buf-seq false)] ;;TODO: not always false
	  (when-not success
	    (throw (Exception. "Bytes left over after decoding sequence of frames.")))
	  (recur remainder (conj vals val)))))))

(defn- decode-byte-sequence [codec reader buf-seq]
  (loop [buf-seq buf-seq, vals [], reader reader]
    (if (empty? buf-seq)
      [vals reader nil]
      (let [[success x remainder] (read-bytes reader buf-seq false)] ;;TODO: not always false
	(if success
	  (recur remainder (conj vals x) codec)
	  [vals x remainder])))))

(defn decode-channel [codec ch]
  (let [src (fork ch)
	dst (channel)]
    (run-pipeline {:reader codec :bytes nil}
      (fn [state]
	(run-pipeline src
	  read-channel
	  (fn [bytes]
	    (if (nil? bytes)
	      (when (closed? src)
		(close dst))
	      
	      (let [bytes (-> bytes to-buf-seq bytes/dup-bytes)
		    [s reader remainder] (decode-byte-sequence
					   codec
					   (:reader state)
					   (bytes/concat-bytes (:bytes state) bytes))]
		(when-not (empty? s)
		  (apply enqueue dst s))
		(when (closed? src)
		  (close dst))
		{:reader reader :bytes (to-buf-seq remainder)})))))
      (fn [x]
	(when-not (closed? src)
	  (restart x))))
    (splice dst nil-channel)))

'(defn decode-stream [codec ^InputStream stream]
  (let [ch (channel)]
    (.start
      (Thread.
	(fn []
	  (loop []
	    (let [ary (byte-array (.available stream))]
	      (.read stream ary)
	      ())))))
    (lazy-channel-seq (decode-channel codec ch))))
