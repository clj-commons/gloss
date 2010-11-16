;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns gloss.core
  (:use
    potemkin)
  (:require
    [gloss.core.protocols :as pr]
    [gloss.core.formats :as formats]
    [gloss.core.frame :as frame]
    [gloss.data.string :as string]
    [gloss.data.bytes :as bytes]
    [gloss.data.primitives :as prim])
  (:import
    [java.nio ByteBuffer]))

(import-fn #'frame/compile-frame)
(import-fn #'formats/to-byte-buffer)
(import-fn #'formats/to-buf-seq)
(import-fn #'pr/read-bytes)
(import-fn #'pr/write-bytes)
(import-fn #'pr/sizeof)

(defn string
  [charset & {:as options}]
  (let [charset (name charset)]
    (cond
      (:length options)
      (string/finite-string-codec charset (:length options))

      (:delimiters options)
      (bytes/wrap-delimited-codec
	(string/string-codec charset)
	(map to-byte-buffer (:delimiters options))
	(or (:strip-delimiters? options) true))

      :else
      (string/string-codec charset))))

(defn header [sig header->body body->header]
  (pr/header
    (compile-frame sig)
    header->body
    body->header))

(defn- repeated-reader [codec len vals]
  (reify
    pr/Reader
    (read-bytes [_ buf-seq]
      (loop [buf-seq buf-seq, vals vals]
	(if (= (count vals) len)
	  [vals buf-seq]
	  (let [[v b] (read-bytes codec buf-seq)]
	    (if (pr/reader? v)
	      [(repeated-reader codec len vals) b]
	      (recur b (conj vals v)))))))))

(defn- prefix-repeated
  [codec]
  (let [codec* (pr/compose-readers
		 (:int32 prim/primitive-codecs)
		 (fn [len b]
		   (read-bytes (repeated-reader codec len []) b)))]
    (if (pr/bounded-writer? codec)
     (reify
       pr/Reader
       (read-bytes [_ buf-seq]
	 (read-bytes codec* buf-seq))
       pr/BoundedWriter
       (sizeof [_ vals]
	 (+ 4 (apply + (map #(sizeof codec %) vals))))
       (write-to-buf [_ buf vals]
	 (.putInt ^ByteBuffer buf (count vals))
	 (doseq [v vals]
	   (pr/write-to-buf codec buf v))))
     (reify
       pr/Reader
       (read-bytes [_ buf-seq]
	 (read-bytes codec* buf-seq))
       pr/UnboundedWriter
       (create-buf [_ vals]
	 (cons
	   (-> (ByteBuffer/allocate 4) (.putInt (count vals)) .rewind)
	   (apply concat (map #(write-bytes codec %) vals))))))))
