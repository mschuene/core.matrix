(ns clojure.core.matrix.stag.stag
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
  ;(let [forms (substitute-symbols tags (eval tag-vals) (eval ts))]
    `(do ~@forms)))

;;;
;;; array wrapper template infrastructure
;;;

;;;
;;; the following code illustrates, in a particular situation,
;;; what the macro below does in general: mask an array implementation
;;; to prevent protocol dispatch from deciding without considering
;;; scalar type carried
;;;
(deftype X [a])
(defprotocol IProtA
  (add [this y]))
(defprotocol IProtB
  (sub [this y]))

(extend-type X
  IProtA
  (add [this y] (+ y (.a this)))
  IProtB
  (sub [this y] (- y (.a this))))

(deftype Hider [hidden]
  IProtA
  (add [this y] (add (.hidden this) y))
  IProtB
  (sub [this y] (sub (.hidden this) y)))

(def x (X. 3))
(add x 1)
(sub x 3)
(def y (Hider. x))
(add y 1)
(sub y 3)
;;;

;; ;; method map
;; (defn build-passthrough
;;   "Builds methods that pass the call to the method
;;   with the same name/arity in the hidden object.
;;   Assumes that mask type has only .hidden field
;;   where it stores the hidden object. This will
;;   trigger double protocol dispatch.
;;   TODO: resolve second dispatch and delegate
;;   directly compile time?"
;;   [[ifc mtd-vec]]
;;   (->> mtd-vec
;;        ; for each method variant create code
;;        (map (fn [[_ name argc]]
;;               (let [argl (take argc (repeatedly gensym))]
;;                 (list (symbol name) (into '[this] argl) (conj argl (list '.hidden 'this)  (symbol (str \. name)))))))
;;        ; stick protocol name in front
;;         (#(conj % (symbol ifc)))))

;; (defn scaffold
;;   "Stripped from Clojure Programming.
;;   Modified to create forms instead
;;   printing them on the stdout.
;;   Grab methods of an interface and
;;   collect them under sub-interfaces."
;;   [interface method-constructor]
;;   (let [payload
;;         (->> interface
;;              .getMethods
;;              (map #(vector (.getName (.getDeclaringClass %))
;;                            (symbol (.getName %))
;;                            (count (.getParameterTypes %))))
;;              (group-by first)
;;              (mapcat #(method-constructor %)))]
;;     payload))

;; (defmacro mask-access
;;   [mask-typename & ifcs]
;;   (let [mts (mapcat #(scaffold (eval %) build-passthrough) ifcs)]
;;      `(deftype ~mask-typename [~'hidden]
;;        ~@mts)))

;; simplified problem with core.matrix container-based protocols
;; (defn add-3
;;   [xo]
;;   (cond
;;    (vector? xo) (+ (first xo) 3)))

;; (add-3 ["1"])

(def pv-metamask (template-lib))
;(add-template pv-metamask
;              (deftype array-mask# [arr-type-hint# a]
;                ))
