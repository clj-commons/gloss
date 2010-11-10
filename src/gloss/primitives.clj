;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns gloss.primitives
  (:use [gloss bytes])
  (:import
    [java.nio
     Buffer
     ByteBuffer]))

(defn has-bytes [n buf-seq]
  (< (.remaining ^Buffer (first buf-seq)) n))

(defmacro take-fn [accessor size]
  `(fn [buf-seq#]
     (cond

       (< (buf-seq-count buf-seq#) ~size)
       [nil buf-seq#]
       
       (has-bytes ~size buf-seq#)
       [(~accessor ^ByteBuffer (first buf-seq#)) buf-seq#]

       :else
       [(~accessor ^ByteBuffer (take-contiguous-bytes ~size buf-seq#))
	(drop-bytes ~size buf-seq#)])))

(def take-fns
  {:byte (take-fn .get 1)
   :int16 (take-fn .getShort 2)
   :int32 (take-fn .getInt 4)
   :int64 (take-fn .getLong 8)
   :float32 (take-fn .getFloat 4)
   :float64 (take-fn .getDouble 8)
   })
