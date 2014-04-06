;; (ns clojure.core.matrix.impl.pv-reals
;;   (:refer-clojure)
;;   (:use clojure.core.matrix.stag.stag)
;;   (:use [clojure.core.matrix.stag.pv-common])
;;   (:require [clojure.core.matrix.stag.pv-generic :as pv-generic])
;;   (:require [clojure.core.matrix.protocols :as mp])
;;   (:use clojure.core.matrix.utils)
;;   (:require [clojure.core.matrix.impl.wrappers :as wrap])
;;   (:require [clojure.core.matrix.implementations :as imp])
;;   (:require [clojure.core.matrix.impl.mathsops :as mops])
;;   (:require [clojure.core.matrix.multimethods :as mm])
;;   (:import clojure.lang.IPersistentVector))
;;   ;(:require [clojure.core.matrix.pv-generic :as t]))

;; (set! *warn-on-reflection* true)
;; (set! *unchecked-math* true)
;; ;;;
;; ;;; instantiate
;; ;;;

;; ;; (count @pvgen/lib)
;; ;; (clojure.pprint/pprint @pvgen/lib)
;; ;; (clojure.pprint/pprint (macroexpand-1 '(pvgen/emit-with-subs
;; ;;                                         [add# sub# mul# div# scalar?# one# zero# =# ># >=# <# <=#]
;; ;;                                         [      +       -       *       /     number?     1.0      0.0    ==     >     >=     <     <=]
;; ;;                                         @pvgen/lib)))

;; ;; helper functin to build generic maths operations
;; (defn build-maths-function
;;   ([[name func]]
;;     `(~name [~'m]
;;             (mapmatrix (fn [x#] (double (~func (double x#)))) ~'m))))

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

;; ;;;
;; ;;; template substitution map
;; ;;;
;; (def tagmap
;;   {'add# +
;;    'sub# =
;;    'mul# *
;;    'div# /
;;    'scalar# number?
;;    'one# 1.0
;;    'zero# 0.0
;;    '=# ==
;;    '># >
;;    '>=# >=
;;    '<# <
;;    '<= <=})

;; ;; will be evaluated in this namespace
;; ;; => all previously defined functions
;; ;;    can be resolved
;; (emit-with-subs [add# sub# mul# div# scalar?# one# zero# =# ># >=# <# <=# sqrt#]
;;                 [   +    -    *    /  number?  1.0   0   == >  >=  <  <=  Math/sqrt]
;;                 @pv-generic/lib)



;; ;; =====================================
;; ;; Register implementation

;; ;(imp/register-implementation [1]) ; what to do about this??
