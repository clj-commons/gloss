;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns gloss.data.primitives
  (:use
    [gloss.data bytes]
    [gloss.core protocols])
  (:import
    [java.nio
     Buffer
     ByteBuffer]))

(defn has-bytes [n buf-seq]
  (< (.remaining ^Buffer (first buf-seq)) n))

(defmacro primitive-handler [accessor writer size]
  `(reify
     ByteConsumer
     (feed [this# buf-seq#]
       (cond

	 (has-bytes ~size buf-seq#)
	 [(~accessor ^ByteBuffer (first buf-seq#))]

	 (< (buf-seq-count buf-seq#) ~size)
	 [this# buf-seq#]

	 :else
	 [(~accessor ^ByteBuffer (take-contiguous-bytes ~size buf-seq#))
	  (drop-bytes ~size buf-seq#)]))
     ByteWriter
     (size [_]
       ~size)
     (write [_ buf# val#]
       (~writer ^ByteBuffer buf# val#))))

(def primitive-handlers
  {:byte (primitive-handler .get .put 1)
   :int16 (primitive-handler .getShort .putShort 2)
   :int32 (primitive-handler .getInt .putInt 4)
   :int64 (primitive-handler .getLong .putLong 8)
   :float32 (primitive-handler .getFloat .putFloat 4)
   :float64 (primitive-handler .getDouble .putDouble 8)})
