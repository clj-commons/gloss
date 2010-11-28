;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns gloss.data.bytes.delimited
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

(defn next-bytes [buf-seq]
  (let [buf ^ByteBuffer (first buf-seq)
	buf (.position buf (inc (.position buf)))]
    (if (.hasRemaining buf)
      buf-seq
      (rest buf-seq))))

(defn match-delimiters [delimiters buf-seq]
  (some
    (fn [^ByteBuffer delimiter]
      (and
	(= (.get delimiter 0) (first-byte buf-seq))
	(or
	  (= 1 (.remaining delimiter))
	  (= delimiter (take-contiguous-bytes (.remaining delimiter) buf-seq)))
	delimiter))
    (dup-bytes delimiters)))

;;naive solution - this assumes small and dissimilar delimiters
(defn take-delimited-bytes
  "Returns the buffer-sequence as two buffer-sequences, split around the first found
   instance of a delimiter."
  ([buf-seq delimiters]
     (take-delimited-bytes delimiters bytes true))
  ([buf-seq delimiters strip-delimiters?]
     (let [delimiters (->> delimiters
			dup-bytes
			(sort #(compare (.remaining ^ByteBuffer %1) (.remaining ^ByteBuffer %2)))
			reverse)]
       (loop [bytes (dup-bytes buf-seq)]
	 (if (empty? bytes)
	   [false nil buf-seq]
	   (if-let [delimiter ^ByteBuffer (match-delimiters delimiters bytes)]
	     (let [delimiter-count (-> delimiter .rewind .remaining)
		   location (- (byte-count buf-seq) (byte-count bytes))]
	       [true
		(take-bytes (+ location (if strip-delimiters? 0 delimiter-count)) buf-seq)
		(drop-bytes (+ location delimiter-count) buf-seq)])
	     (recur (next-bytes bytes))))))))

(defn delimited-bytes-codec
  ([delimiters strip-delimiters?]
     (delimited-bytes-codec nil delimiters strip-delimiters?))
  ([scanned delimiters strip-delimiters?]
     (let [delimiters (dup-bytes delimiters)
	   max-delimiter-size (apply max
				(map
				  #(.remaining ^ByteBuffer %)
				  delimiters))
	   split-index (- (byte-count scanned) (dec max-delimiter-size))
	   prefix (drop-bytes split-index scanned)
	   scanned (take-bytes split-index scanned)]
       (reify
	 Reader
	 (read-bytes [this b]
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
  (let [delimiters (dup-bytes delimiters)
	delimited-codec (compose-callback
			  (delimited-bytes-codec delimiters true)
			  (fn [bytes remainder]
			    (let [[success v remainder*] (read-bytes codec bytes)]
			      (assert success)
			      (assert (empty? remainder*))
			      [true v remainder])))]
    (reify
      Reader
      (read-bytes [_ b]
	(read-bytes delimited-codec b))
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
  (let [delimiters (dup-bytes delimiters)
	suffix (first delimiters)
	sizeof-delimiter (.remaining ^Buffer suffix)
	read-codec (compose-callback
		     (delimited-bytes-codec delimiters true)
		     (take-all codec))]
    (reify
      Reader
      (read-bytes [_ b]
	(read-bytes read-codec b))
      Writer
      (sizeof [_]
	nil)
      (write-bytes [_ buf vs]
	(if (sizeof codec)
	  (with-buffer [buf (+ sizeof-delimiter (* (count vs) (sizeof codec)))]
	    (doseq [v vs]
	      (write-bytes codec buf v))
	    (.put ^ByteBuffer buf (duplicate suffix)))
	  (apply concat
	    (map #(write-bytes codec buf %) vs)
	    [(duplicate suffix)]))))))

