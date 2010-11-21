;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns gloss.io
  (:use
    [gloss.core codecs structure protocols formats]
    [lamina core]))

(defn encoder-channel [frame]
  (let [ch (channel)]
    (splice
      (map* #(write-bytes frame nil %) ch)
      ch)))

(defn decode-stream [codec reader buf-seq]
  (loop [buf-seq buf-seq, vals [], reader reader]
    (if (empty? buf-seq)
      [vals codec nil]
      (let [[success x remainder] (read-bytes reader buf-seq)]
	(if success
	  (recur remainder (conj vals x) codec)
	  [vals x remainder])))))

(defn decoder-channel [codec]
  (let [src (channel)
	dst (channel)]
    (run-pipeline {:reader codec :bytes nil}
      (fn [state]
	(run-pipeline dst
	  read-channel
	  (fn [bytes]
	    (let [[s reader remainder] (decode-stream
					 codec
					 (:reader state)
					 (concat (:bytes state) (to-buf-seq bytes)))]
	      (when-not (empty? s)
		(apply enqueue src s))
	      {:reader reader :bytes remainder}))))
      restart)
    (splice src dst)))
