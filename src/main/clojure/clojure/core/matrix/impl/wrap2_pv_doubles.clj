(ns clojure.core.matrix.impl.wrap2-pv-doubles
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

(deftype Wrapper2 [^clojure.lang.IPersistentVector w]
  clojure.lang.Seqable
;    (seq [this] (seq ^clojure.lang.IPersistentVector (.w this)))
    ;(seq [this] (map #(.nth this %) (range (.length this))))
    (seq [this] (map #(if (vector? %)
                        (Wrapper2. %)
                        %) (seq ^clojure.lang.IPersistentVector (.w this))))
  clojure.lang.IPersistentCollection
    ;(count [this] (count (.w this)))
    (cons [this x] (Wrapper2. (cons (.w this) x)))
    (empty [this] (Wrapper2. (empty (.w this))))
    (equiv [this x] (if (instance? Wrapper2 x)
                               (.equiv ^clojure.lang.IPersistentVector (.w this) (.w ^Wrapper2 x))
                               (.equiv ^clojure.lang.IPersistentVector (.w this) x)))
  clojure.lang.Reversible
    (rseq [this] (rseq ^clojure.langi.IPersistentVector (.w this)))
  clojure.lang.ILookup
    (valAt [this x] (.valAt ^clojure.lang.IPersistentVector (.w this) x))
    (valAt [this x y] (.valAt ^clojure.lang.IPersistentVector (.w this) x y))
  clojure.lang.IPersistentVector
    (length [this] (.length ^clojure.lang.IPersistentVector (.w this)))
    ;(cons [this x] (Wrapper2. (cons (.w this) x)))
    (assocN [this x y] (Wrapper2. (.assocN ^clojure.lang.IPersistentVector (.w this) x y)))
  clojure.lang.Counted
    (count [this] (count ^clojure.lang.IPersistentVector (.w this)))
  clojure.lang.Associative
    (containsKey [this k] (.containsKey ^clojure.lang.IPersistentVector (.w this) k))
    (entryAt [this n] (.entryAt ^clojure.lang.IPersistentVector (.w this) n))
    (assoc [this x y] (assoc ^clojure.langi.IPersistentVector (.w this) x y))
  clojure.lang.Indexed
    (nth [this x] (let [n (nth (.w this) x)]
                    (if (vector? n)
                      (Wrapper2. n)
                      n)))
    (nth [this x y] (let [n (nth (.w this) x y)]
                      (if (vector? n)
                        (Wrapper2. n)
                        n)))
  clojure.lang.IPersistentStack
    (pop [this] (Wrapper2. (pop (.w this))))
    (peek [this] (peek (.w this))))

(defn pv-wrap
  [w]
  (Wrapper2. (vec w)))

(defn sub-pv-wrap
  ([^Wrapper2 pvw st] (Wrapper2. (subvec ^clojure.lang.IPersistentVector (.w pvw) st)))
  ([^Wrapper2 pvw st en] (Wrapper2. (subvec ^clojure.lang.IPersistentVector (.w pvw) st en))))

(Wrapper2. [1 2 3])
(seq (Wrapper2. [1 2 3]))
(time (dotimes [_ 1000000] (= (Wrapper2. [1]) [0])))
(time (dotimes [_ 1000000] (= [1] [0])))
(count (Wrapper2. [1 2 3]))
(.length [1 2 3])
(.length (Wrapper2. [1 2 3]))

(def X (Wrapper2. [1 2 3]))
;(subvec X 1 2)
(def Y (Wrapper2. [:a :b]))
(concat X Y)
(concat [1 2 3] [:a :b])
(vector? X)

(class (sub-pv-wrap X 1 2))
(emit-with-subs [add# sub# mul# div#
                 scalar?# one# zero#
                 =# ># >=# <# <=# sqrt#
                 array-wrap-type-hint#
                 array-wrap-type#
                 container-factory#
                 sub-container#
                 impl-key#]
                [* - * /
                 number? 1.0 0.0
                 == > >= < <=  Math/sqrt
                 Wrapper2
                 Wrapper2
                 pv-wrap
                 sub-pv-wrap
                 :wrapper2]
                @wpvg/lib)



;; (count @wpvg/lib)

;; =====================================
;; Register implementation

;(imp/register-implementation [1.0])
(imp/register-implementation (Wrapper2. [1.0]))
