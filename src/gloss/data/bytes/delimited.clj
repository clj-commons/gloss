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
    gloss.core.formats
    clojure.contrib.def
    clojure.pprint
    clojure.walk)
  (:import
    [java.nio
     Buffer
     ByteBuffer]))

(defn first-byte [buf-seq]
  (let [buf ^ByteBuffer (first buf-seq)]
    (.get buf (.position buf))))

(defn sort-delimiters [delimiters]
  (->> delimiters
    (map rewind)
    (map duplicate)
    (sort #(compare (.remaining ^ByteBuffer %1) (.remaining ^ByteBuffer %2)))
    reverse))

(defn byte-buffer->byte-seq [^ByteBuffer buf]
  (let [buf (.rewind buf)]
    (take (.remaining buf) (repeatedly #(.get buf)))))

(defn buf->string [buf-seq]
  (let [buf-seq (dup-bytes (to-buf-seq buf-seq))]
    (map #(apply str (map char (take (.remaining %) (repeatedly (fn [] (.get %)))))) buf-seq)))

(defn split-buf-seq [buf-seq min-delimiter-length max-delimiter-length]
  (let [buf-seq (dup-bytes buf-seq)
	offset (dec max-delimiter-length)
	dimensions (butlast (reductions + (map #(.remaining ^ByteBuffer %) buf-seq)))]
    (doall
      (concat
	(mapcat
	  (fn [[n ^ByteBuffer buf]]
	    (let [len (.remaining buf)]
	      [(when (>= len max-delimiter-length)
		 [(- n len) buf])
	       [(max 0 (- n offset))
		(-> buf-seq
		  (drop-bytes (- n offset))
		  (take-contiguous-bytes (* 2 offset)))]]))
	  (map vector dimensions (butlast buf-seq)))
	(let [last-buf ^ByteBuffer (last buf-seq)]
	  (when (>= (.remaining last-buf) min-delimiter-length)
	    [[(or (last dimensions) 0)
	      (last buf-seq)]]))))))

(defn delimiter-matcher [delimiter max-delimiter-length]
  (let [delimiter (byte-buffer->byte-seq delimiter)]
    `(do
       (.position ~'buf ~'pos)
       (and
	~@(when (< 1 max-delimiter-length)
	    [`(<= ~'pos (or ~'final-position
			  (unchecked-subtract ~'buf-length ~(count delimiter))))])
	~@(map
	    (fn [val] `(== ~(int val) (int (.get ~'buf))))
	    delimiter)))))

(defn match-loop [delimiters strip-delimiters?]
  (let [delimiters (sort-delimiters delimiters)
	max-delimiter-length (.remaining ^ByteBuffer (first delimiters))]
    (eval
      (postwalk-replace
	{'buf (with-meta 'buf {:tag "java.nio.ByteBuffer"})}
	`(fn [~'buf last-and-complete?#]
	   (let [~'buf-length (.remaining ~'buf)
		 ~'final-position (when-not last-and-complete?#
				    (unchecked-subtract ~'buf-length ~max-delimiter-length))]
	     (loop [~'pos 0]
	       (if (== ~'pos ~'buf-length)
		 [false 0 0]
		 (if (or ~@(map #(delimiter-matcher % max-delimiter-length) delimiters))
		   [true ~(if strip-delimiters? 'pos `(.position ~'buf)) (.position ~'buf)]
		   (recur (unchecked-inc ~'pos)))))))))))

(defn-memo delimited-bytes-splitter [delimiters strip-delimiters?]
  (let [match-fn (match-loop delimiters strip-delimiters?)
	delimiter-lengths (map #(.remaining ^ByteBuffer %) delimiters)
	max-delimiter-length (apply max delimiter-lengths)
	min-delimiter-length (apply min delimiter-lengths)]
    (if (= 1 max-delimiter-length)

      (fn [buf-seq]
	(if (empty? buf-seq)
	  [false nil buf-seq]
	  (loop [offset 0, bufs (dup-bytes buf-seq)]
	   (if (empty? bufs)
	     [false nil (rewind-bytes buf-seq)]
	     (let [buf ^ByteBuffer (first bufs)]
	       (let [[success end start] (match-fn buf false)]
		 (if success
		   (let [buf-seq (rewind-bytes buf-seq)]
		     [true
		      (take-bytes buf-seq (+ offset end))
		      (drop-bytes buf-seq (+ offset start))])
		   (recur (unchecked-add offset (-> buf .rewind .remaining)) (rest bufs)))))))))

      (fn [buf-seq]
	(let [complete? complete?]
	  (if (empty? buf-seq)
	    [false nil buf-seq]
	    (loop [bufs (split-buf-seq buf-seq min-delimiter-length max-delimiter-length)]
	      (if (empty? bufs)
		[false nil (rewind-bytes buf-seq)]
		(let [[offset buf] (first bufs)]
		  (let [[success end start]
			(when buf
			  (match-fn buf (and complete? (= 1 (count bufs)))))]
		    (if success
		      (let [buf-seq (rewind-bytes buf-seq)]
			[true
			 (take-bytes buf-seq (+ offset end))
			 (drop-bytes buf-seq (+ offset start))])
		      (recur (rest bufs)))))))))))))

(defn delimited-bytes-codec
  ([delimiters strip-delimiters?]
     (delimited-bytes-codec
       nil
       (delimited-bytes-splitter delimiters strip-delimiters?)
       delimiters
       strip-delimiters?))
  ([scanned take-delimited-bytes delimiters strip-delimiters?]
     (let [scanned (to-buf-seq scanned)
	   max-delimiter-length (apply max (map #(.remaining ^ByteBuffer %) delimiters))
	   split-index (- (byte-count scanned) (dec max-delimiter-length))
	   prefix (drop-bytes scanned split-index)
	   scanned (take-bytes scanned split-index)]
       (reify
	 Reader
	 (read-bytes [this b]
	   (let [[success x xs] (take-delimited-bytes (to-buf-seq (concat prefix b)))]
	     (if success
	       [true (to-buf-seq (concat scanned x)) xs]
	       [false
		(delimited-bytes-codec
		  (concat scanned prefix b)
		  take-delimited-bytes
		  delimiters
		  strip-delimiters?)
		nil])))
	 Writer
	 (sizeof [_]
	   nil)
	 (write-bytes [_ _ v]
	   (concat v [(duplicate (first delimiters))]))))))

(defn delimited-codec
  ([delimiters codec]
     (delimited-codec delimiters true codec))
  ([delimiters strip-delimiters? codec]
     (let [delimiters (map duplicate delimiters)
	   delimited-codec (compose-callback
			     (delimited-bytes-codec delimiters strip-delimiters?)
			     (fn [bytes remainder]
			       (let [[success v remainder*]
				     (binding [complete? true]
				       (read-bytes codec (to-buf-seq bytes)))]
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
	     [(duplicate (first delimiters))]))))))

(defn wrap-delimited-sequence
  ([delimiters codec]
     (wrap-delimited-sequence delimiters true codec))
  ([delimiters strip-delimiters? codec]
     (let [delimiters (map duplicate delimiters)
	   suffix (first delimiters)
	   sizeof-delimiter (.remaining ^Buffer suffix)
	   read-codec (compose-callback
			(delimited-bytes-codec delimiters strip-delimiters?)
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
	     (concat
	       (mapcat #(write-bytes codec buf %) vs)
	       [(duplicate suffix)])))))))

