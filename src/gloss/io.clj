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

(defn- decode-stream [codec reader buf-seq]
  (loop [buf-seq buf-seq, vals [], reader reader]
    (if (empty? buf-seq)
      [vals codec nil]
      (let [[success x remainder] (read-bytes reader buf-seq)]
	(if success
	  (recur remainder (conj vals x) codec)
	  [vals x remainder])))))

(defn decoder-channel [codec ch]
  (let [src (fork ch)
	dst (channel)]
    (run-pipeline {:reader codec :bytes nil}
      (fn [state]
	(run-pipeline src
	  read-channel
	  (fn [bytes]
	    (if-not bytes
	      (if-not (closed? src)
		(enqueue dst nil)
		(enqueue-and-close dst nil))
	      (let [[s reader remainder] (decode-stream
					   codec
					   (:reader state)
					   (concat (:bytes state) (to-buf-seq bytes)))]
		(if (closed? src)
		  (enqueue-and-close dst (when-not (empty? s) s))
		  (when-not (empty? s)
		    (apply enqueue dst s)))
		{:reader reader :bytes remainder})))))
      (fn [x]
	(when-not (closed? src)
	  (restart x))))
    (splice dst nil-channel)))
