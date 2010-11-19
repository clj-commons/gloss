;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns gloss.io
  (:use
    [gloss.core frame protocols formats]
    [lamina core]))

(defn encoder-channel [frame]
  (let [ch (channel)]
    (splice
      (map* #(write-bytes frame nil %) ch)
      ch)))

(defn decoder-channel [frame]
  (let [src (channel)
	dst (channel)]
    (run-pipeline nil
      (fn [remainder]
	(run-pipeline dst
	  read-channel
	  (fn [bytes]
	    (let [[s remainder] (frame-seq
				  frame
				  (concat remainder (to-buf-seq bytes)))]
	      (apply enqueue src s)
	      remainder))))
      restart)
    (splice src dst)))
