(ns clojure.core.matrix.stag.pv-generic
  (:use clojure.core.matrix.stag.gen)
  (:require [clojure.core.matrix.protocols :as mp])
  (:use clojure.core.matrix.utils)
  (:require [clojure.core.matrix.impl.wrappers :as wrap])
  (:require [clojure.core.matrix.implementations :as imp])
  (:require [clojure.core.matrix.impl.mathsops :as mops])
  (:require [clojure.core.matrix.multimethods :as mm])
  (:import clojure.lang.IPersistentVector))


;; create a lib
(def lib (template-lib))

;; (def form-silly '(defn xxx [x] (add# x 3)))
;; (macroexpand '(emit-forms [add#] [+] (defn xxx [x] (add# x 3))))
;; (emit-forms '[add#] '[-] form-silly)
;; (macroexpand-1 '(inst [add#] [+] form-silly))
;; (xxx 2)
;(macroexpand-1 '(emit-with-subs [zero#] [0.0] @lib))

;;;
;;; template definitions
;;;

;form_0
(add-template lib
              (extend-protocol mp/PImplementation
                IPersistentVector
                (implementation-key [m] :persistent-vector)
                (meta-info [m]
                  {:doc "Implementation for nested Clojure persistent vectors
                        used as matrices"})
                (new-vector [m length] (vec (repeat length zero#)))
                (new-matrix [m rows columns] (vec (repeat rows (mp/new-vector m columns))))
                (new-matrix-nd [m dims]
                  (if-let [dims (seq dims)]
                    (vec (repeat (first dims) (mp/new-matrix-nd m (next dims))))
                    zero#))
                (construct-matrix [m data]
                  (persistent-vector-coerce data))
                (supports-dimensionality? [m dims]
                  true)))
;form_1
(add-template lib
              (extend-protocol mp/PMatrixAdd
                IPersistentVector
                (matrix-add [m a]
                  (let [[m a] (mp/broadcast-compatible m a)]
                    (mapmatrix add# m (persistent-vector-coerce a))))
                (matrix-sub [m a]
                  (let [[m a] (mp/broadcast-compatible m a)]
                    (mapmatrix sub# m (persistent-vector-coerce a))))))
;form_2
(add-template lib
              (extend-protocol mp/PVectorOps
                IPersistentVector
                (vector-dot [a b]
                  (let [dims (long (mp/dimensionality b))
                        ;; b (persistent-vector-coerce b)
                        ]
                    (cond
                      (and (== dims 1) (instance? clojure.lang.Indexed b))
                      (do
                        (when-not (== (count a) (count b)) (error "Mismatched vector sizes"))
                        (reduce add# zero# (map mul# a b)))
                      (== dims 0) (mp/scale a b)
                      :else (mp/inner-product a b))))
                (length [a]
                ;needs serious fixing
                (sqrt# (double (reduce add# (map #(mul# % %) a)))))
                (length-squared [a]
                  ;needs serious fixing
                  (reduce add# (map #(mul# % %) a)))
                (normalise [a]
                  ;needs serious fixing
                  (mp/scale a (/ one# (sqrt# (mp/length-squared a)))))))
;form_3
(add-template lib
              (extend-protocol mp/PVectorDistance
                IPersistentVector
                ; needs serious fixing
                  (distance [a b]
                            (mp/length (mapv sub# b a)))))

;form_4
(add-template lib
              (extend-protocol mp/PSummable
                IPersistentVector
                (element-sum [a]
                  (mp/element-reduce a add#))))

;form_5
(add-template lib
              (extend-protocol mp/PMatrixEquality
                IPersistentVector
                (matrix-equals [a b]
                  (let [bdims (long (mp/dimensionality b))]
                    (cond
                      (<= bdims 0)
                      false
                      (not= (count a) (mp/dimension-count b 0))
                      false
                      (== 1 bdims)
                      (and (== 1 (mp/dimensionality a))
                           (let [n (long (count a))]
                             (loop [i 0]
                               (if (< i n)
                                 (if (=# (mp/get-1d a i) (mp/get-1d b i))
                                   (recur (inc i))
                                   false)
                                 true))))
                      (vector? b)
                      (let [n (long (count a))]
                        (loop [i 0]
                          (if (< i n)
                            (if (mp/matrix-equals (a i) (b i))
                              (recur (inc i))
                              false)
                            true)))
                      :else
                      (loop [sa (seq a) sb (mp/get-major-slice-seq b)]
                        (if sa
                          (if (mp/matrix-equals (first sa) (first sb))
                            (recur (next sa) (next sb))
                            false)
                          true)))))))

;form_6
(add-template lib
              (extend-protocol mp/PMatrixMultiply
                IPersistentVector
                (element-multiply [m a]
                  (if (scalar?# a)
                    (mp/scale m a)
                    (let [[m a] (mp/broadcast-compatible m a)]
                      (mp/element-map m mul# a))))
                (matrix-multiply [m a]
                  (let [mdims (long (mp/dimensionality m))
                        adims (long (mp/dimensionality a))]
                    (cond
                      (== adims 0) (mp/scale m a)
                      (and (== mdims 1) (== adims 2))
                      (vec (for [i (range (mp/dimension-count a 1))]
                             (let [r (mp/get-column a i)]
                               (mp/vector-dot m r))))
                      (and (== mdims 2) (== adims 1))
                      (mapv #(mp/vector-dot % a) m)
                      (and (== mdims 2) (== adims 2))
                      (mapv (fn [r]
                              (vec (for [j (range (mp/dimension-count a 1))]
                                     (mp/vector-dot r (mp/get-column a j))))) m)
                      :else
                      (mm/mul m a))))))

;form_7
(add-template lib
              (extend-protocol mp/PMatrixScaling
                IPersistentVector
                (scale [m a]
                  (let [a (mp/get-0d a)]
                    (mapmatrix #(mul# % a) m)))
                (pre-scale [m a]
                  (let [a (mp/get-0d a)]
                    (mapmatrix #(mul# a %) m)))))

;form_8
(add-template lib
              (extend-protocol mp/PSquare
                IPersistentVector
                (square [m]
                  (mapmatrix #(mul# % %) m))))


