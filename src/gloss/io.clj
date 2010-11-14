;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns gloss.io
  (:require
    [clojure.zip :as z])
  (:use
    [clojure.walk]
    [gloss.data primitives string bytes]
    [gloss.core protocols])
  (:import
    [java.nio
     Buffer
     ByteBuffer]))




;;;

(defn value? [x]
  (or (emitter? x) (writer? x)))

(defn flatten-values [frame val]
  (->> (map list (flatten frame) (flatten val))
    (filter #(value? (first %)))
    (map second)))



;;;



(defn consume-zipper [zip buf-seq]
  (loop [zip zip, buf-seq buf-seq]
    (let [n (z/node zip)
	  n (if (fn? n) (n))]
      )))
