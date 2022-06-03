;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^{:skip-wiki true}
  gloss.data.bytes
  (:require
    [gloss.core.formats :refer :all]
    [gloss.core.protocols :refer :all]
    [gloss.data.bytes.delimited :as delimited]
    [gloss.data.bytes.core :as core]
    [gloss.data.bytes.bits :as bits]
    [potemkin :refer :all])
  (:import
    [gloss.data.bytes.core
     SingleBufferSequence
     MultiBufferSequence]))

(import-fn core/create-buf-seq)
(import-fn core/duplicate)

(import-fn core/byte-count)
(import-fn core/dup-bytes)
(import-fn core/take-bytes)
(import-fn core/drop-bytes)
(import-fn core/take-contiguous-bytes)
(import-fn core/rewind-bytes)
(import-fn core/concat-bytes)

(import-fn delimited/delimited-codec)
(import-fn delimited/wrap-delimited-sequence)
(import-fn delimited/delimited-bytes-codec)
(import-fn delimited/buf->string)

(import-fn bits/bit-seq)

(defn single-buffer? [x]
  (instance? SingleBufferSequence x))

(defn finite-byte-codec
  [len]
  (reify
    Reader
    (read-bytes [this b]
      (if (< (byte-count b) len)
        [false this b]
        [true (take-bytes b len) (drop-bytes b len)]))
    Writer
    (sizeof [_]
      len)
    (write-bytes [_ buf v]
      (when-let [v (-> v to-buf-seq dup-bytes)]
        (if-not buf
          v
          (core/write-to-buf v buf))))))

(defn wrap-finite-block
  [prefix-codec codec]
  (let [read-codec (compose-callback
                     prefix-codec
                     (fn [len b]
                       (if (zero? len)
                         [true nil b]
                         (read-bytes
                           (compose-callback
                             (finite-byte-codec len)
                             (fn [v b]
                               (let [[success v b*]
                                     (binding [complete? true]
                                       (read-bytes codec v))]
                                 (assert success)
                                 (assert (empty? b*))
                                 [true v b])))
                           b))))]
    (reify
      Reader
      (read-bytes [_ b]
        (read-bytes read-codec b))
      Writer
      (sizeof [_]
        nil)
      (write-bytes [_ buf v]
        (let [buf-seq (core/create-buf-seq (write-bytes codec nil v))]
          (concat
            (write-bytes prefix-codec nil (byte-count buf-seq))
            buf-seq))))))



 
