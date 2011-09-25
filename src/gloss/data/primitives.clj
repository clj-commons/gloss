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

(defn bits [x num-bits]
  (map #(if (pos? (bit-and (bit-shift-left 1 %) x)) 1 0) (range num-bits)))

(defn byte->ubyte
  [x]
  (bit-and 0xFF (Short. (short x))))

(defn short->ushort
  [x]
  (bit-and 0xFFFF (Integer. (int x))))

(defn int->uint
  [x]
  (bit-and 0xFFFFFFFF (Long. (long x))))
  
(defn long->ulong
  [x]
  (bit-and 0xFFFFFFFFFFFFFFFF (bigint x)))

(defn ubyte->byte
  [x]
  (.byteValue (Short. (short x))))

(defn ushort->short
  [x]
  (.shortValue (Integer. (int x))))

(defn uint->int
  [x]
  (.intValue (Long. (long x))))

(defn ulong->long
  [x]
  (.longValue (bigint x)))


(defmacro primitive-codec [accessor writer size get-transform typecast put-transform]
  `(reify
     Reader
     (read-bytes [this# b#]
       (if (< (byte-count b#) ~size)
	 [false this# b#]
	 (let [first-buf# (first b#)
	       remaining# (.remaining ^Buffer first-buf#)]
	   (cond
	     (= ~size remaining#)
	     [true
	      (~get-transform (~accessor ^ByteBuffer first-buf#))
	      (rest b#)]
	     
	     (< ~size remaining#)
	     [true
	      (~get-transform (~accessor ^ByteBuffer first-buf#))
	      (-> b# rewind-bytes (drop-bytes ~size))]
	     
	     :else
	     (let [buf# (take-contiguous-bytes b# ~size)]
	       [true
		(~get-transform (~accessor ^ByteBuffer buf#))
		(drop-bytes b# ~size)])))))
     Writer
     (sizeof [_]
       ~size)
     (write-bytes [_ buf# v#]
       (with-buffer [buf# ~size]
	 (~writer ^ByteBuffer buf# (~typecast (~put-transform v#)))))))

(def primitive-codecs
  {:byte (primitive-codec .get .put 1 identity byte to-byte) 
   :int16 (primitive-codec .getShort .putShort 2 identity short identity)
   :int32 (primitive-codec .getInt .putInt 4 identity int identity)
   :int64 (primitive-codec .getLong .putLong 8 identity long identity)
   :float32 (primitive-codec .getFloat .putFloat 4 identity float identity)
   :float64 (primitive-codec .getDouble .putDouble 8 identity double identity)
   :ubyte (primitive-codec .get .put 1 byte->ubyte byte ubyte->byte)
   :uint16 (primitive-codec .getShort .putShort 2 short->ushort short ushort->short)
   :uint32 (primitive-codec .getInt .putInt 4 int->uint int uint->int)
   :uint64 (primitive-codec .getLong .putLong 8 long->ulong long ulong->long)})
