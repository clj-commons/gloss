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

(defn sizeof [frame value]
  (when (pr/bounded-writer? frame)
    (pr/sizeof frame value)))

(defn delimited-block [delimiters frame]
  (bytes/delimited-block delimiters false (compile-frame frame)))

(defn string
  [charset & {:as options}]
  (let [charset (name charset)]
    (cond
      (:length options)
      (string/finite-string-codec charset (:length options))

      (:delimiters options)
      (bytes/delimited-block
	(string/string-codec charset)
	(map to-byte-buffer (:delimiters options)))

      :else
      (string/string-codec charset))))

(defn header [frame header->body body->header]
  (pr/header
    (compile-frame frame)
    header->body
    body->header))

;;;

(defn repeated [frame & {:as options}]
  (let [codec (compile-frame frame)]
    (cond
      (:delimiters options)
      (bytes/wrap-delimited-sequence
	codec
	(map to-byte-buffer (:delimiters options)))
      
      :else
      (pr/wrap-prefix-repeated
	((or (:prefix options) :int32) prim/primitive-codecs)
	codec))))
