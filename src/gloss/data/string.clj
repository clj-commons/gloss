;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^{:skip-wiki true}
  gloss.data.string
  (:require
    [potemkin :refer :all])
  (:require
    [gloss.data.string.core :as core]
    [gloss.data.string.codecs :as codecs]))

(import-fn core/take-chars)
(import-fn core/drop-chars)
(import-fn core/rewind-chars)
(import-fn core/create-char-sequence)

(import-fn codecs/string-codec)

