(ns clojure.core.matrix.impl.stag-pv-doubles
  (:refer-clojure)
  (:use clojure.core.matrix.stag.stag)
  (:require [clojure.core.matrix.protocols :as mp])
  (:use clojure.core.matrix.utils)
  (:require [clojure.core.matrix.impl.wrappers :as wrap])
  (:require [clojure.core.matrix.implementations :as imp])
  (:require [clojure.core.matrix.impl.mathsops :as mops])
  (:require [clojure.core.matrix.multimethods :as mm])
  (:require [clojure.core.matrix.stag.wrap-pv-generic :as wpvg])
  (:import clojure.lang.IPersistentVector))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)
;;;
;;; instantiate
;;;

;; ;; helper functin to build generic maths operations
;; (defn build-maths-function
;;   ([[name func]]
;;     `(~name [~'m]
;;             (wpvg/mapmatrix (fn [x#] (double (~func (double x#)))) ~'m))))

;; ;; code generation for maths functions
;; ;; we generate both name and name! versions
;; (eval
;;   `(extend-protocol mp/PMathsFunctions
;;      clojure.lang.IPersistentVector
;;        ~@(map build-maths-function mops/maths-ops)
;;        ~@(map (fn [[name func]]
;;                 (let [name (str name "!")
;;                       mname (symbol name)
;;                       mpmname (symbol "clojure.core.matrix.protocols" name)]
;;                   `(~mname [m#]
;;                      (doseq [s# (mp/get-major-slice-seq m#)]
;;                        (~mpmname s#)))))
;;               mops/maths-ops)))


;; (defmacro vector-1d? [pv]
;;   `(let [^clojure.lang.IPersistentVector pv# ~pv]
;;      (or (== 0 (.length pv#)) (== 0 (mp/dimensionality (.nth pv# 0))))))

;; will be evaluated in this namespace
;; => all above defined functions can
;;    be resolved
(emit-with-subs [add# sub# mul# div# scalar?# one# zero# =# ># >=# <# <=# sqrt#       array-wrap-type-hint#  array-wrap-type#]
                [   +    -    *    /  number?  1.0   0   == >  >=  <  <=  Math/sqrt clojure.lang.IPersistentVector IPersistentVector]
;                [   +    -    *    /  number?  1.0   0   == >  >=  <  <=  Math/sqrt Wrapper Wrapper]
                @wpvg/lib)

(count @wpvg/lib)

;; (count @wpvg/dtl)
;; (macroexpand-1 '(emit-with-subs [add# sub# mul# div# scalar?# one# zero# =# ># >=# <# <=# sqrt#       array-wrap-type-hint#  array-wrap-type#]
;;                 [   +    -    *    /  number?  1.0   0.0   == >  >=  <  <=  Math/sqrt clojure.lang.IPersistentVector IPersistentVector]
;;                 @wpvg/dtl))
;; (emit-with-subs [add# sub# mul# div# scalar?# one# zero# =# ># >=# <# <=# sqrt#       array-wrap-type-hint#  array-wrap-type#]
;;                 [   +    -    *    /  number?  1.0   0.0   == >  >=  <  <=  Math/sqrt clojure.lang.IPersistentVector IPersistentVector]
;;                 @wpvg/dtl)


;; =====================================
;; Register implementation

(imp/register-implementation [1.0])
;(imp/register-implementation (Wrapper. [1.0]))
