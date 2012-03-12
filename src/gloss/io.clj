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
    [lamina core api])
  (:require
    [gloss.core.formats :as formats]
    [gloss.data.bytes :as bytes])
  (:import
    [java.nio.channels
     Channels]
    [java.nio
     ByteBuffer]
    [java.io
     InputStream
     OutputStream]))

;;;

(import-fn formats/to-byte-buffer)
(import-fn formats/to-buf-seq)

(defn ^ByteBuffer contiguous
  "Takes a sequence of ByteBuffers and returns a single contiguous ByteBuffer."
  [buf-seq]
  (when-let [buf-seq (-> buf-seq to-buf-seq bytes/dup-bytes)]
    (bytes/take-contiguous-bytes buf-seq (bytes/byte-count buf-seq))))

;;;

(defn encode
  "Turns a frame value into a sequence of ByteBuffers."
  [frame val]
  (let [codec (compile-frame frame)]
    (write-bytes codec nil val)))

(defn encode-to-buffer
  "Encodes a sequence of values, and writes them to a ByteBuffer."
  [frame buf vals]
  (let [codec (compile-frame frame)]
    (assert (sizeof codec))
    (doseq [v vals]
      (write-bytes codec buf v))))

(defn encode-all
  "Turns a sequence of frame values into a sequence of ByteBuffers."
  [frame vals]
  (let [codec (compile-frame frame)]
    (if-let [size (sizeof codec)]
      (let [buf (ByteBuffer/allocate (* size (count vals)))]
	(encode-to-buffer codec buf vals)
	[(.rewind buf)])
      (apply concat
	(map #(write-bytes codec nil %) vals)))))

(defn encode-to-stream
  "Encodes a sequence of values, and writes them to an OutputStream."
  [frame ^OutputStream output-stream vals]
  (let [codec (compile-frame frame)
	channel (Channels/newChannel output-stream)]
    (doseq [buf (encode-all codec vals)]
      (.write channel ^ByteBuffer buf))))

;;;

(defn decode
  "Turns bytes into a single frame value.  If there are too few or too many bytes
   for the frame, an exception is thrown."
  [frame bytes]
  (let [codec (compile-frame frame)]
    (binding [complete? true]
      (let [buf-seq (bytes/dup-bytes (to-buf-seq bytes))
	    [success val remainder] (read-bytes codec buf-seq)]
	(when-not success
	  (throw (Exception. "Insufficient bytes to decode frame.")))
	(when-not (zero? (bytes/byte-count remainder))
	  (throw (Exception. "Bytes left over after decoding frame.")))
	val))))

(defn- decoder [frame]
  (let [codec (compile-frame frame)]
    (fn [buf-seq]
      (when-not (zero? (bytes/byte-count buf-seq))
        (let [[success & rest] (read-bytes codec buf-seq)]
          (when-not success
            (throw (Exception. "Bytes left over after decoding sequence of frames.")))
          rest)))))

(defn decode-all
  "Turns bytes into a sequence of frame values.  If there are bytes left over at the end
   of the sequence, an exception is thrown."
  [frame bytes]
  (let [decode-next (decoder frame)]
    (binding [complete? true]
      (loop [buf-seq (bytes/dup-bytes (to-buf-seq bytes))
             vals    []]
        (if-let [[val remainder] (decode-next buf-seq)]
          (recur remainder (conj vals val))
          vals)))))

(defn lazy-decode-all
  "Turns bytes into a lazy sequence of frame values.  If there are bytes left over at the
   end of the sequence, an exception is thrown."
  [frame bytes]
  (let [decode-next (decoder frame)]
    ((fn decode-rest [buf-seq]
       (lazy-seq
         (binding [complete? true]
           (when-let [[val remainder] (decode-next buf-seq)]
             (cons val (decode-rest remainder))))))
     (bytes/dup-bytes (to-buf-seq bytes)))))

(defn- decode-byte-sequence [codecs buf-seq]
  (if (empty? buf-seq)
    (let [[success x remainder] (read-bytes (first codecs) buf-seq)]
      (if success
	[[x] (rest codecs) remainder]
	[nil (cons x (rest codecs)) remainder]))
    (loop [buf-seq buf-seq, vals [], codecs codecs]
      (if (or (empty? codecs) (zero? (bytes/byte-count buf-seq)))
	[vals codecs buf-seq]
	(let [[success x remainder] (read-bytes (first codecs) buf-seq)]
	  (if success
	    (recur remainder (conj vals x) (rest codecs))
	    [vals (cons x (rest codecs)) remainder]))))))

(defn decode-channel
  "Given a channel that emits bytes, returns a channel that emits decoded frames whenever
   there are sufficient bytes."
  [src frame]
  (let [src (copy src)
	dst (channel)
	codec (compile-frame frame)]
    (on-closed dst #(close src))
    (run-pipeline {:codecs (repeat codec) :bytes nil}
      :error-handler (fn [_] (close dst))
      (fn [state]
	(run-pipeline (read-channel src)
	  (fn [bytes]
	    (binding [complete? (drained? src)]
	      (let [bytes (-> bytes to-buf-seq bytes/dup-bytes)
		    [s codecs remainder] (when-not (zero? (bytes/byte-count bytes))
					   (decode-byte-sequence
					     (:codecs state)
					     (bytes/concat-bytes (:bytes state) bytes)))]
		(when-not (empty? s)
		  (apply enqueue dst s))
		(when (drained? src)
		  (close dst))
		{:codecs codecs :bytes (to-buf-seq remainder)})))))
      (fn [x]
	(when-not (drained? src)
	  (restart x))))
    (splice dst nil-channel)))

(defn decode-channel-headers
  "Given a channel that emits bytes, returns a channel that will emit one decoded frame for
   each frame passed into the function.  After those frames have been decoded, the channel will
   simply emit any bytes that are passed into the source channel."
  [src & frames]
  (let [dst (channel)]
    (run-pipeline {:codecs (map compile-frame frames) :bytes nil}
      (fn [state]
	(run-pipeline (read-channel src)
	  (fn [bytes]
	    (if (empty? (:codecs state))
	      state
	      (binding [complete? (drained? src)]
		(let [bytes (-> bytes to-buf-seq bytes/dup-bytes)
		      [vals codecs remainder] (decode-byte-sequence
						(:codecs state)
						(bytes/concat-bytes (:bytes state) bytes))]
		  (when-not (empty? vals)
		    (apply enqueue dst vals))
		  {:codecs codecs, :bytes (to-buf-seq remainder)}))))))
      (fn [state]
	(cond
	  (empty? (:codecs state))
	  (do
	    (when-let [remainder (:bytes state)]
	      (enqueue dst remainder))
	    (siphon src dst)
	    (on-drained src #(close dst)))

	  (not (drained? src))
	  (restart state))))
    (splice dst nil-channel)))
