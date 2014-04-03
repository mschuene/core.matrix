(ns clojure.core.matrix.gen
  (:refer-clojure)
  (:require [clojure.template :as tt]))

;;;
;;; infrastructure for defining and collecting templates
;;;
(defn template-lib [] (atom []))
; using vector to preserve order of defs

(defmacro add-template [lib template]
  `(swap! ~lib conj '~template))

;;;
;;; instantiation infrastructure
;;;

(defn substitute-symbols
  [tags tag-vals ts]
  (map #(tt/apply-template tags % tag-vals) ts))

(defmacro emit-with-subs
  [tags tag-vals ts]
  (let [forms (substitute-symbols tags tag-vals (eval ts))]
    `(do ~@forms)))