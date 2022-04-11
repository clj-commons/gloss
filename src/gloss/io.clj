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
    [potemkin])
  (:require
    [manifold.deferred :as d]
    [manifold.stream :as s]
    [gloss.core.formats :as formats]
    [gloss.data.bytes :as bytes])
  (:import
    [gloss.data.bytes.core
     SingleBufferSequence]
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

(defn encode-to-buffer
  "Encodes a sequence of values, and writes them to a ByteBuffer."
  [frame buf vals]
  (let [codec (compile-frame frame)]
    ;(assert (sizeof codec))
    (doseq [v vals]
      (write-bytes codec buf v))))

(defn encode
  "Turns a frame value into a sequence of ByteBuffers."
  ([frame val]
   (encode frame val false))
  ([frame val direct?]
   (let [codec (compile-frame frame)]
     (if direct?
       (let [size (sizeof codec)
             buf (ByteBuffer/allocateDirect size)]
         (write-bytes codec buf val)
         (SingleBufferSequence. (.rewind ^ByteBuffer buf) size))
       (write-bytes codec nil val)))))

(defn encode-all
  "Turns a sequence of frame values into a sequence of ByteBuffers."
  ([frame vals]
   (encode-all frame vals false))
  ([frame vals direct?]
   (let [codec (compile-frame frame)]
     (if-let [size (sizeof codec)]
       (let [buf (if direct?
                   (ByteBuffer/allocateDirect (* size (count vals)))
                   (ByteBuffer/allocate (* size (count vals))))]
         (encode-to-buffer codec buf vals)
         [(.rewind buf)])
       (apply concat
         (map #(write-bytes codec nil %) vals))))))

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
  ([frame bytes]
     (decode frame bytes true))
  ([frame bytes no-remainder?]
     (let [codec (compile-frame frame)]
       (binding [complete? true]
         (let [buf-seq (bytes/dup-bytes (to-buf-seq bytes))
               [success val remainder] (read-bytes codec buf-seq)]
           (when-not success
             (throw (Exception. "Insufficient bytes to decode frame.")))
           (when (and no-remainder? (not (zero? (bytes/byte-count remainder))))
             (throw (Exception. "Bytes left over after decoding frame.")))
           val)))))

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

(defn decode-stream
  "Given a stream that emits bytes, returns a channel that emits decoded frames whenever
   there are sufficient bytes."
  [src frame]
  (let [src (s/->source src)
        dst (s/stream)
        state-ref (atom {:codecs (repeat frame) :bytes nil})
        f (fn [bytes]
            (let [state @state-ref]
              (binding [complete? (s/drained? src)]
                (let [bytes (-> bytes to-buf-seq bytes/dup-bytes)
                      [s codecs remainder] (decode-byte-sequence
                                             (:codecs state)
                                             (bytes/concat-bytes (:bytes state) bytes))]
                  (reset! state-ref {:codecs codecs :bytes (to-buf-seq remainder)})
                  (s/put-all! dst s)))))]

    (s/connect-via src f dst {:downstream? false})
    (s/on-drained src #(do (f []) (s/close! dst)))

    dst))

(def decode-channel decode-stream)

(defn decode-stream-headers
  "Given a channel that emits bytes, returns a channel that will emit one decoded frame for
   each frame passed into the function.  After those frames have been decoded, the channel will
   simply emit any bytes that are passed into the source channel."
  [src & frames]
  (let [src (s/->source src)
        dst (s/stream)
        state-ref (atom {:codecs (map compile-frame frames) :bytes nil})
        f (fn [bytes]
            (let [{:keys [codecs] :as state} @state-ref]
              (if (empty? codecs)
                (s/put! dst bytes)
                (binding [complete? (s/drained? src)]
                  (let [bytes (-> bytes to-buf-seq bytes/dup-bytes)
                        [s codecs remainder] (decode-byte-sequence
                                               codecs
                                               (bytes/concat-bytes (:bytes state) bytes))]
                    (reset! state-ref {:codecs codecs :bytes (to-buf-seq remainder)})
                    (let [res (s/put-all! dst s)]
                      (if (empty? codecs)
                        (s/put-all! dst remainder)
                        res)))))))]

    (s/connect-via src f dst {:downstream? false})
    (s/on-drained src #(do (f []) (s/close! dst)))

    dst))

(def decode-channel-headers decode-stream-headers)
