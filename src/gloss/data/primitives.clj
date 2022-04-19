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
     ByteBuffer
     ByteOrder]))

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

(defn long->byte-array [^long n]
  (-> (ByteBuffer/allocate 8) (.putLong n) .array))

(defn long->ulong
  [x]
  (let [^bytes magnitude (long->byte-array x)]
    (bigint (BigInteger. 1 magnitude))))

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

(def byte-order
  {:le `ByteOrder/LITTLE_ENDIAN
   :be `ByteOrder/BIG_ENDIAN
   :ne `(ByteOrder/nativeOrder)})

(defn- native-byte-order []
  (if (= (ByteOrder/nativeOrder) ByteOrder/LITTLE_ENDIAN)
    :le
    :be))

(defn bytes->umedium
  [x bo]
  (let [bo-trans (if (= :le bo) reverse identity)
        [b0 b1 b2] (bo-trans x)]
    (bit-or
     (bit-or
      (bit-shift-left (bit-and b0 0xFF) 16)
      (bit-shift-left (bit-and b1 0xFF) 8))
     (bit-shift-left (bit-and b2 0xFF) 0))))

(defn umedium->medium
  [x]
  (let [uval2 (bit-and x 0x800000)]
    (if (not= uval2 0)
      (unchecked-int (bit-or x 0xff000000))
      x)))

(defn medium->bytes
  [x bo]
  (let [bo-trans (if (= :le bo) reverse identity)]
    (into-array Byte/TYPE
                (bo-trans
                 (map unchecked-byte
                      [(bit-shift-right x 16)
                       (bit-shift-right x 8)
                       x])))))

(defn get-unsigned-medium [bo buffer]
  (let [bytes (byte-array 3)]
    (.get ^ByteBuffer buffer bytes)
    (bytes->umedium bytes bo)))

(defn put-medium [bo buffer value]
  (let [bytes (medium->bytes value bo)]
    (.put ^ByteBuffer buffer ^bytes bytes)))

(defmacro with-byte-order [[buf bo] & body]
  (if (nil? bo)
    `(do ~@body)
    `(let [^ByteBuffer buf# ~buf
           obo# (.order buf#)]
       (try
         (.order buf# ~(byte-order bo))
         ~@body
         (finally
           (.order buf# obo#))))))

(defmacro primitive-codec
  [accessor writer size get-transform typecast put-transform & optional]
  (let [[bo] optional]
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
               (with-byte-order [first-buf# ~bo]
                 (~get-transform (~accessor ^ByteBuffer first-buf#)))
               (rest b#)]

              (< ~size remaining#)
              [true
               (with-byte-order [first-buf# ~bo]
                 (~get-transform (~accessor ^ByteBuffer first-buf#)))
               (-> b# rewind-bytes (drop-bytes ~size))]

              :else
              (let [buf# (take-contiguous-bytes b# ~size)]
                [true
                 (with-byte-order [buf# ~bo]
                   (~get-transform (~accessor ^ByteBuffer buf#)))
                 (drop-bytes b# ~size)])))))
       Writer
       (sizeof [_]
         ~size)
       (write-bytes [_ buf# v#]
         (with-buffer [buf# ~size]
           (with-byte-order [buf# ~bo]
             (~writer ^ByteBuffer buf# (~typecast (~put-transform v#)))))))))



(def primitive-codecs
  {:byte (primitive-codec .get .put 1 identity byte to-byte)

   :int16 (primitive-codec .getShort .putShort 2 identity short identity)
   :int16-le (primitive-codec .getShort .putShort 2 identity short identity :le)
   :int16-be (primitive-codec .getShort .putShort 2 identity short identity :be)

   :int24 (primitive-codec (partial get-unsigned-medium (native-byte-order))
                           (partial put-medium (native-byte-order)) 3 umedium->medium int identity)
   :int24-le (primitive-codec (partial get-unsigned-medium :le)
                              (partial put-medium :le) 3 umedium->medium int identity :le)
   :int24-be (primitive-codec (partial get-unsigned-medium :be)
                              (partial put-medium :be) 3 umedium->medium int identity :be)

   :int32 (primitive-codec .getInt .putInt 4 identity int identity)
   :int32-le (primitive-codec .getInt .putInt 4 identity int identity :le)
   :int32-be (primitive-codec .getInt .putInt 4 identity int identity :be)

   :int64 (primitive-codec .getLong .putLong 8 identity long identity)
   :int64-le (primitive-codec .getLong .putLong 8 identity long identity :le)
   :int64-be (primitive-codec .getLong .putLong 8 identity long identity :be)

   :float32 (primitive-codec .getFloat .putFloat 4 identity float identity)
   :float32-le (primitive-codec .getFloat .putFloat 4 identity float identity :le)
   :float32-be (primitive-codec .getFloat .putFloat 4 identity float identity :be)

   :float64 (primitive-codec .getDouble .putDouble 8 identity double identity)
   :float64-le (primitive-codec .getDouble .putDouble 8 identity double identity :le)
   :float64-be (primitive-codec .getDouble .putDouble 8 identity double identity :be)

   :ubyte (primitive-codec .get .put 1 byte->ubyte byte ubyte->byte)

   :uint16 (primitive-codec .getShort .putShort 2 short->ushort short ushort->short)
   :uint16-le (primitive-codec .getShort .putShort 2 short->ushort short ushort->short :le)
   :uint16-be (primitive-codec .getShort .putShort 2 short->ushort short ushort->short :be)

   :uint24 (primitive-codec (partial get-unsigned-medium (native-byte-order))
                            (partial put-medium (native-byte-order)) 3 identity int identity)
   :uint24-le (primitive-codec (partial get-unsigned-medium :le)
                               (partial put-medium :le) 3 identity int identity :le)
   :uint24-be (primitive-codec (partial get-unsigned-medium :be)
                               (partial put-medium :be) 3 identity int identity :be)

   :uint32 (primitive-codec .getInt .putInt 4 int->uint int uint->int)
   :uint32-le (primitive-codec .getInt .putInt 4 int->uint int uint->int :le)
   :uint32-be (primitive-codec .getInt .putInt 4 int->uint int uint->int :be)

   :uint64 (primitive-codec .getLong .putLong 8 long->ulong long ulong->long)
   :uint64-le (primitive-codec .getLong .putLong 8 long->ulong long ulong->long :le)
   :uint64-be (primitive-codec .getLong .putLong 8 long->ulong long ulong->long :be)})
