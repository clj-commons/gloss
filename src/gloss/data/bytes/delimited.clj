;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^{:skip-wiki true}
  gloss.data.bytes.delimited
  (:use
    gloss.data.bytes.core
    gloss.core.protocols
    gloss.core.formats)
  (:import
    [java.nio
     Buffer
     ByteBuffer]))

(defn first-byte [buf-seq]
  (let [buf ^ByteBuffer (first buf-seq)]
    (.get buf (.position buf))))

(defn sort-delimiters [delimiters]
  (->> delimiters
    (map duplicate)
    (sort #(compare (.remaining ^ByteBuffer %1) (.remaining ^ByteBuffer %2)))
    reverse))

(defn match-delimiters [delimiters buf-seq]
  (loop [delimiters delimiters]
    (when-not (empty? delimiters)
      (let [delimiter ^ByteBuffer (first delimiters)]
	(if (and
	      (= (.get delimiter 0) (first-byte buf-seq))
	      (or
		(= 1 (.remaining delimiter))
		(= delimiter (take-contiguous-bytes buf-seq (.remaining delimiter)))))
	  delimiter
	  (recur (rest delimiters)))))))

;;naive solution - this assumes small and dissimilar delimiters
(defn take-delimited-bytes
  "Returns the buffer-sequence as two buffer-sequences, split around the first found
   instance of a delimiter."
  ([buf-seq delimiters]
     (take-delimited-bytes buf-seq delimiters true))
  ([buf-seq delimiters strip-delimiters?]
     (let [buf-seq (to-buf-seq buf-seq)]
       (loop [bytes (dup-bytes buf-seq)]
	 (if (nil? bytes)
	   [false nil buf-seq]
	   (if-let [delimiter ^ByteBuffer (match-delimiters delimiters bytes)]
	     (let [delimiter-count (-> delimiter .rewind .remaining)
		   location (- (byte-count buf-seq) (byte-count bytes))]
	       [true
		(take-bytes buf-seq (+ location (if strip-delimiters? 0 delimiter-count)))
		(drop-bytes buf-seq (+ location delimiter-count))])
	     (recur (drop-bytes bytes 1))))))))

(defn delimited-bytes-codec
  ([delimiters strip-delimiters?]
     (delimited-bytes-codec nil delimiters strip-delimiters?))
  ([scanned delimiters strip-delimiters?]
     (let [delimiters (sort-delimiters delimiters)
	   max-delimiter-size (apply max
				(map
				  #(.remaining ^ByteBuffer %)
				  delimiters))
	   scanned (create-buf-seq scanned)
	   split-index (- (byte-count scanned) (dec max-delimiter-size))
	   prefix (drop-bytes scanned split-index)
	   scanned (take-bytes scanned split-index)]
       (reify
	 Reader
	 (read-bytes [this b bounded?]
	   (let [[success x xs] (take-delimited-bytes (concat prefix b) delimiters strip-delimiters?)]
	     (if success
	       [true (concat scanned x) xs]
	       [false (delimited-bytes-codec (concat scanned prefix b) delimiters strip-delimiters?) nil])))
	 Writer
	 (sizeof [_]
	   nil)
	 (write-bytes [_ _ v]
	   (concat v [(duplicate (first delimiters))]))))))

(defn delimited-codec
  [delimiters codec]
  (let [delimiters (map duplicate delimiters)
	delimited-codec (compose-callback
			  (delimited-bytes-codec delimiters true)
			  (fn [bytes remainder _]
			    (let [[success v remainder*] (read-bytes codec (to-buf-seq bytes) true)]
			      (assert success)
			      (assert (empty? remainder*))
			      [true v remainder])))]
    (reify
      Reader
      (read-bytes [_ b bounded?]
	(read-bytes delimited-codec b bounded?))
      Writer
      (sizeof [_]
	nil)
      (write-bytes [_ buf v]
	(concat
	  (if (sizeof codec)
	    (with-buffer [buf (sizeof codec)]
	      (write-bytes codec buf v))
	    (write-bytes codec buf v))
	  [(duplicate (first delimiters))])))))

(defn wrap-delimited-sequence
  [delimiters codec]
  (let [delimiters (map duplicate delimiters)
	suffix (first delimiters)
	sizeof-delimiter (.remaining ^Buffer suffix)
	read-codec (compose-callback
		     (delimited-bytes-codec delimiters true)
		     (take-all codec))]
    (reify
      Reader
      (read-bytes [_ b bounded?]
	(read-bytes read-codec b bounded?))
      Writer
      (sizeof [_]
	nil)
      (write-bytes [_ buf vs]
	(if (sizeof codec)
	  (with-buffer [buf (+ sizeof-delimiter (* (count vs) (sizeof codec)))]
	    (doseq [v vs]
	      (write-bytes codec buf v))
	    (.put ^ByteBuffer buf (duplicate suffix)))
	  (concat
	    (mapcat #(write-bytes codec buf %) vs)
	    [(duplicate suffix)]))))))

