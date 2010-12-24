;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^{:skip-wiki true}
  gloss.data.primitives
  (:use
    [gloss.data bytes]
    [gloss.core protocols])
  (:import
    [java.nio
     Buffer
     ByteBuffer]))

(defn has-bytes [n buf-seq]
  (< (.remaining ^Buffer (first buf-seq)) n))

(defn to-byte [x]
  (cond
    (number? x) (byte x)
    (char? x) (-> x int byte)
    (string? x) (-> x first int byte)
    :else (throw (Exception. (str "Cannot convert " x " to byte.")))))

(defmacro primitive-codec [accessor writer size typecast transform-fn]
  `(reify
     Reader
     (read-bytes [this# b#]
       (if-let [first-buf# (first b#)]
	 (let [remaining# (.remaining ^Buffer first-buf#)]
	   (cond
	     (= ~size remaining#)
	     [true (~accessor ^ByteBuffer (first b#)) (rest b#)]
	     
	     (< ~size remaining#)
	     [true (~accessor ^ByteBuffer (first b#)) b#]
	     
	     :else
	     (if-let [buf# (take-contiguous-bytes ~size b#)]
	       [true (~accessor ^ByteBuffer buf#) (drop-bytes ~size b#)]
	       [false this# b#])))
	 [false this# b#]))
     Writer
     (sizeof [_]
       ~size)
     (write-bytes [_ buf# v#]
       (with-buffer [buf# ~size]
	 (~writer ^ByteBuffer buf# (~typecast (~transform-fn v#)))))))

(def primitive-codecs
  {:byte (primitive-codec .get .put 1 byte to-byte) 
   :int16 (primitive-codec .getShort .putShort 2 short identity)
   :int32 (primitive-codec .getInt .putInt 4 int identity)
   :int64 (primitive-codec .getLong .putLong 8 long identity)
   :float32 (primitive-codec .getFloat .putFloat 4 float identity)
   :float64 (primitive-codec .getDouble .putDouble 8 double identity)})
