(ns clojure.core.matrix.impl.wrap-pv-doubles
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

(deftype Wrapper [^clojure.lang.IPersistentVector w]
  clojure.lang.Seqable
    (seq [this] (seq ^clojure.lang.IPersistentVector (.w this)))
  clojure.lang.IPersistentCollection
    ;(count [this] (count (.w this)))
    (cons [this x] (Wrapper. (cons (.w this) x)))
    (empty [this] (Wrapper. (empty (.w this))))
    (equiv [^Wrapper this x] (if (instance? Wrapper x)
                               (.equiv ^clojure.lang.IPersistentVector (.w this) (.w ^Wrapper x))
                               (.equiv ^clojure.lang.IPersistentVector (.w this) x)))
  clojure.lang.Reversible
    (rseq [this] (rseq ^clojure.langi.IPersistentVector (.w this)))
  clojure.lang.ILookup
    (valAt [this x] (.valAt ^clojure.lang.IPersistentVector (.w this) x))
    (valAt [this x y] (.valAt ^clojure.lang.IPersistentVector (.w this) x y))
  clojure.lang.IPersistentVector
    (length [this] (.length ^clojure.lang.IPersistentVector (.w this)))
    ;(cons [this x] (Wrapper. (cons (.w this) x)))
    (assocN [this x y] (Wrapper. (.assocN ^clojure.lang.IPersistentVector (.w this) x y)))
  clojure.lang.Counted
    (count [this] (count ^clojure.lang.IPersistentVector (.w this)))
  clojure.lang.Associative
    (containsKey [this k] (.containsKey ^clojure.lang.IPersistentVector (.w this) k))
    (entryAt [this n] (.entryAt ^clojure.lang.IPersistentVector (.w this) n))
    (assoc [this x y] (assoc ^clojure.langi.IPersistentVector (.w this) x y))
  clojure.lang.Indexed
    (nth [this x] (nth (.w this) x))
    (nth [^Wrapper this x y] (nth (.w this) x y))
  clojure.lang.IPersistentStack
    (pop [this] (Wrapper. (pop (.w this))))
    (peek [this] (peek (.w this))))

(defn mk-pv-wrapper
  [^clojure.lang.IPersistentVector w]
  (Wrapper. w))

(Wrapper. [1 2 3])
(seq (Wrapper. [1 2 3]))
(time (dotimes [_ 1000000] (= (Wrapper. [1]) [0])))
(time (dotimes [_ 1000000] (= [1] [0])))
(count (Wrapper. [1 2 3]))
(.length [1 2 3])
(.length (Wrapper. [1 2 3]))

;(clojure.pprint/pprint (first @wpvg/lib))
;(emit-with-subs [array-wrap-type-hint#] [clojure.lang.IPersistentVector] (first @wpvg/lib))
;(macroexpand-1 '(emit-with-subs [add# sub# mul# div# scalar?# one# zero# =# ># >=# <# <=# sqrt#              array-wrap-type-hint#  array-wrap-type#]
;                [   +    -    *    /  number?  1.0   0   == >  >=  <  <=  Math/sqrt clojure.lang.IPersistentVector IPersistentVector]
;                @lib))

;; (defmacro vector-1d? [pv]
;;   `(let [^clojure.lang.IPersistentVector pv# ~pv]
;;      (or (== 0 (.length pv#)) (== 0 (mp/dimensionality (.nth pv# 0))))))

;; will be evaluated in this namespace
;; => all above defined functions can
;;    be resolved
(emit-with-subs [add# sub# mul# div# scalar?# one# zero# =# ># >=# <# <=# sqrt#       array-wrap-type-hint#  array-wrap-type#]
                [   *    -    *    /  number?  1.0   0   == >  >=  <  <=  Math/sqrt clojure.lang.IPersistentVector IPersistentVector]
;                [   +    -    *    /  number?  1.0   0   == >  >=  <  <=  Math/sqrt Wrapper Wrapper]
                @wpvg/lib)

(count @wpvg/lib)

;; =====================================
;; Register implementation

(imp/register-implementation [1.0])
;(imp/register-implementation (Wrapper. [1.0]))
