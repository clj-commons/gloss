;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns gloss.flow)

(defprotocol ByteConsumer
  (feed [buf-seq]))

(defn consume-seq [consumer buf-seq]
  (loop [result [], buf-seq buf-seq]
    (let [[x xs] (feed consumer buf-seq)]
      (if x
	(recur (conj result x) xs)
	result))))

