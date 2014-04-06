(ns clojure.core.matrix.stag.wrap-pv-generic
  (:refer-clojure)
  (:use clojure.core.matrix.stag.stag)
  (:use clojure.core.matrix.utils)
  (:require [clojure.core.matrix.protocols :as mp])
  (:require [clojure.core.matrix.impl.wrappers :as wrap])
  (:require [clojure.core.matrix.implementations :as imp])
  (:require [clojure.core.matrix.impl.mathsops :as mops])
  (:require [clojure.core.matrix.multimethods :as mm]))
;  (:import clojure.lang.IPersistentVector))


(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

;; create a lib
(def lib (template-lib))
(def dtl (template-lib)) ; for debug

;; (def form-silly '(defn xxx [x] (add# x 3)))
;; (macroexpand '(emit-forms [add#] [+] (defn xxx [x] (add# x 3))))
;; (emit-forms '[add#] '[-] form-silly)
;; (macroexpand-1 '(inst [add#] [+] form-silly))
;; (xxx 2)
;(macroexpand-1 '(emit-with-subs [zero#] [0.0] @lib))

;;;
;;; template definitions
;;;

(add-template lib
              (declare persistent-vector-coerce))

(add-template lib
              (defn coerce-nested
                "Ensures a vector is fully coerced to nested persistent vectors"
                ([v]
                 (mapv persistent-vector-coerce v))))

(add-template lib
              (defmacro vector-1d? [pv]
                (let [tpv (vary-meta (gensym) assoc :tag array-wrap-type-hint#)]
                  `(let [~tpv ~pv]
                     (or (== 0 (.length ~tpv)) (== 0 (mp/dimensionality (.nth ~tpv 0))))))))

(add-template lib
              (defn mapmatrix
                "Maps a function over all components of a persistent vector matrix. Like mapv but for matrices.
                Assumes correct dimensionality / shape.

                Returns a nested persistent vector matrix or a scalar value."
                ([f m]
                 (let [dims (long (mp/dimensionality m))]
                   (cond
                     (== 0 dims) (f (scalar-coerce m))
                     (== 1 dims) (mapv #(f (scalar-coerce %)) m)
                     :else (mapv (partial mapmatrix f) m))))
                ([f m1 m2]
                 (let [dim2 (long (mp/dimensionality m2))]
                   (cond (mp/is-vector? m1)
                         (do
                           (when (> dim2 1) (error "mapping with array of higher dimensionality?"))
                           (when (and (== 1 dim2) (not= (mp/dimension-count m1 0) (mp/dimension-count m2 0))) (error "Incompatible vector sizes"))
                           (if (== 0 dim2)
                             (let [v (scalar-coerce m2)] (mapv #(f % v) m1 ))
                             (mapv f m1 (mp/element-seq m2))))
                         :else
                         (mapv (partial mapmatrix f)
                               m1
                               (mp/get-major-slice-seq m2)))))
                ([f m1 m2 & more]
                 (if (mp/is-vector? m1)
                   (apply mapv f m1 m2 more)
                   (apply mapv (partial mapmatrix f) m1 m2 more)))))

;; original form
;; (add-template lib
;;               (defn mapv-identity-check
;;                 "Maps a function over a persistent vector, only modifying the vector if the function
;;                 returns a different value"
;;                 ([f ^clojure.lang.IPersistentVector v]
;;                  (let [n (.count v)]
;;                    (loop [i 0 v v]
;;                      (if (< i n)
;;                        (let [x (.nth v i)
;;                              y (f x)]
;;                          (recur (inc i) (if (identical? x y) v (assoc v i y))))
;;                        v))))))

(add-template lib
              (eval
                (let [tv (vary-meta (gensym) assoc :tag (quote array-wrap-type-hint#))]
                `(defn ~'mapv-identity-check
                   "Maps a function over a persistent vector, only modifying the vector if the function
                   returns a different value"
                   ([~'f ~tv]
                    (let [n# (~'.count ~tv)]
                      ;(println "YYY")
                      (loop [~'i 0 ~'v ~tv]
                        (if (< ~'i n#)
                            ~'(let [x (.nth v i)
                                  y (f x)]
                              (recur (inc i) (if (identical? x y) v (assoc v i y))))
                            ~'v))))))))


(add-template lib
              (defn check-vector-shape
                ([v shape]
                 ;(println "ZZZ")
                 (and
                   (instance? array-wrap-type# v)
                   (== (count v) (first shape))
                   (if-let [ns (next shape)]
                     (every? #(check-vector-shape % ns) v)
                     (every? #(not (instance? array-wrap-type# %)) v))))))

(add-template lib (defn is-nested-persistent-vectors?
                    "Test if array is already in nested persistent vector array format."
                    ([x]
                     (cond
                       (number? x) true
                       (mp/is-scalar? x) true
                       (not (instance? array-wrap-type# x)) false
                       :else (and
                               (every? is-nested-persistent-vectors? x)
                               (check-vector-shape x (mp/get-shape x)))))))

(add-template lib
              (defn persistent-vector-coerce [x]
                "Coerces to nested persistent vectors"
                (let [dims (long (mp/dimensionality x))]
                  (cond
                    (> dims 0) (mp/convert-to-nested-vectors x) ;; any array with 1 or more dimensions
                    (and (== dims 0) (not (mp/is-scalar? x))) (mp/get-0d x) ;; array with zero dimensionality

                    ;; it's not an array - so try alternative coercions
                    (nil? x) x
                    (.isArray (class x)) (map persistent-vector-coerce (seq x))
                    (instance? java.util.List x) (coerce-nested x)
                    (instance? java.lang.Iterable x) (coerce-nested x)
                    (sequential? x) (coerce-nested x)

                    ;; treat as a scalar value
                    :default x))))

(add-template lib
              (eval
                (let [thm (vary-meta (gensym) assoc :tag (quote array-wrap-type-hint#))]
                  `(defn ~'vector-dimensionality [~thm]
                     "Calculates the dimensionality (== nesting depth) of nested persistent vectors"
                     ;(println "QQQ")
                     (cond
                       (clojure.core/vector? ~thm)
                       (if (> (count ~thm) 0)
                         (+ 1 (~'vector-dimensionality (.nth ~thm 0)))
                         1)
                       :else (mp/dimensionality ~thm))))))

;; =======================================================================
;; Implementation for nested Clojure persistent vectors used as matrices
(add-template lib
              (extend-protocol mp/PImplementation
                array-wrap-type#
                ;(implementation-key [m] (keyword array-wrap-type#))
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

(add-template lib
              (extend-protocol mp/PBroadcast
                array-wrap-type#
                (broadcast [m target-shape]
                  (let [mshape (mp/get-shape m)
                        dims (long (count mshape))
                        tdims (long (count target-shape))]
                    (cond
                      (> dims tdims)
                      (error "Can't broadcast to a lower dimensional shape")
                      (not (every? identity (map #(== %1 %2) mshape (take-last dims target-shape))))
                      (error "Incompatible shapes, cannot broadcast " (vec mshape) " to " (vec target-shape))
                      :else
                      (reduce
                        (fn [m dup] (vec (repeat dup m)))
                        m
                        (reverse (drop-last dims target-shape))))))))

(add-template lib
              (extend-protocol mp/PBroadcastLike
                array-wrap-type#
                (broadcast-like [m a]
                  (mp/broadcast a (mp/get-shape m)))))

(add-template lib
              (extend-protocol mp/PBroadcastCoerce
                array-wrap-type#
                (broadcast-coerce [m a]
                  (mp/broadcast (persistent-vector-coerce a) (mp/get-shape m)))))

(add-template lib
              (extend-protocol mp/PIndexedAccess
                array-wrap-type#
                (get-1d [m x]
                  (let [r (.nth m (int x))]
                    (scalar-coerce r)))
                (get-2d [m x y]
                  (let [row (.nth m (int x))]
                    (mp/get-1d row y)))
                (get-nd [m indexes]
                  (if-let [indexes (seq indexes)]
                    (if-let [next-indexes (next indexes)]
                      (let [m (.nth m (int (first indexes)))]
                        (mp/get-nd m next-indexes))
                      (.nth m (int (first indexes))))
                    m))))

;; we extend this so that nested mutable implementions are possible
(add-template lib
              (extend-protocol mp/PIndexedSetting
                array-wrap-type#
                (set-1d [m row v]
                  (assoc m row v))
                (set-2d [m row column v]
                  (assoc m row (mp/set-1d (m row) column v)))
                (set-nd [m indexes v]
                  (if-let [indexes (seq indexes)]
                    (let [fi (first indexes)]
                      (if (== 1 (count indexes))
                        (assoc m fi v)
                        (assoc m fi (mp/set-nd (m fi) (next indexes) v))))
                    (error "Trying to set on a persistent vector with insufficient indexes?")))
                (is-mutable? [m]
                  false)))

(add-template lib
              (extend-protocol mp/PMatrixSlices
                array-wrap-type#
                (get-row [m i]
                  (.nth m (long i)))
                (get-column [m i]
                  (mp/get-slice m 1 i))
                (get-major-slice [m i]
                  (let [sl (.nth m (long i))]
                    sl))
                (get-slice [m dimension i]
                  (let [dimension (long dimension)]
                    (if (== dimension 0)
                      (mp/get-major-slice m i)
                      (let [sd (dec dimension)]
                        (mapv #(mp/get-slice % sd i) m)))))))

(add-template lib
              (extend-protocol mp/PSliceView
                array-wrap-type#
                (get-major-slice-view [m i] (.nth m i))))

(add-template lib
              (extend-protocol mp/PSliceSeq
                array-wrap-type#
                (get-major-slice-seq [m]
                  (if (vector-1d? m)
                    (seq (map mp/get-0d m))
                    (seq m)))))

(add-template lib
              (extend-protocol mp/PSliceJoin
                array-wrap-type#
                (join [m a]
                  (let [dims (mp/dimensionality m)
                        adims (mp/dimensionality a)]
                    (cond
                      (== dims adims)
                      (vec (concat (mp/get-major-slice-seq m) (mp/get-major-slice-seq a)))
                      (== dims (inc adims))
                      (conj m a)
                      :else
                      (error "Joining with array of incompatible size"))))))

(add-template lib
              (extend-protocol mp/PRotate
                array-wrap-type#
                (rotate [m dim places]
                  (if (== 0 dim)
                    (let [c (count m)
                          sh (if (> c 0) (mod places c) 0)]
                      (if (== sh 0)
                        m
                        (vec (concat (subvec m sh c) (subvec m 0 sh)))))
                    (mapv (fn [s] (mp/rotate s (dec dim) places)) m)))))

(add-template lib
              (extend-protocol mp/PSubVector
                array-wrap-type#
                (subvector [m start length]
                  (subvec m start (+ start length)))))

(add-template lib
              (extend-protocol mp/PValidateShape
                array-wrap-type#
                (validate-shape [m]
                  (if (mp/same-shapes? m)
                    (mp/get-shape m)
                    (error "Inconsistent shape for persistent vector array.")))))

(add-template lib
              (extend-protocol mp/PMatrixAdd
                array-wrap-type#
                (matrix-add [m a]
                  (let [[m a] (mp/broadcast-compatible m a)]
                    (mapmatrix add# m (persistent-vector-coerce a))))
                (matrix-sub [m a]
                  (let [[m a] (mp/broadcast-compatible m a)]
                    (mapmatrix sub# m (persistent-vector-coerce a))))))

(add-template lib
              (extend-protocol mp/PVectorOps
                array-wrap-type#
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

(add-template lib
              (extend-protocol mp/PMutableMatrixConstruction
                array-wrap-type#
                (mutable-matrix [m]
                  nil ;; fall-though: should get an ndarray result
                  )))

(add-template lib
              (extend-protocol mp/PVectorDistance
                array-wrap-type#
                ; needs serious fixing
                  (distance [a b]
                            (mp/length (mapv sub# b a)))))

(add-template lib
              (extend-protocol mp/PSummable
                array-wrap-type#
                (element-sum [a]
                  (mp/element-reduce a add#))))

(add-template lib
              (extend-protocol mp/PCoercion
                array-wrap-type#
                (coerce-param [m param]
                  (persistent-vector-coerce param))))

(add-template lib
              (extend-protocol mp/PMatrixEquality
                array-wrap-type#
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

(add-template lib
              (extend-protocol mp/PMatrixMultiply
                array-wrap-type#
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

(add-template lib
              (extend-protocol mp/PVectorTransform
                array-wrap-type#
                (vector-transform [m a]
                  (mp/matrix-multiply m a))
                (vector-transform! [m a]
                  (mp/assign! a (mp/matrix-multiply m a)))))

(add-template lib
              (extend-protocol mp/PMatrixScaling
                array-wrap-type#
                (scale [m a]
                  (let [a (mp/get-0d a)]
                    (mapmatrix #(mul# % a) m)))
                (pre-scale [m a]
                  (let [a (mp/get-0d a)]
                    (mapmatrix #(mul# a %) m)))))

(add-template lib
              (extend-protocol mp/PSquare
                array-wrap-type#
                (square [m]
                  (mapmatrix #(mul# % %) m))))

(add-template lib
              (extend-protocol mp/PRowOperations
                array-wrap-type#
                (swap-rows [m i j]
                  (if (== i j)
                    m
                    (assoc (assoc m i (m j)) j (m i))))
                (multiply-row [m i factor]
                  (assoc m i (mp/scale (m i) factor)))
                (add-row [m i j factor]
                  (assoc m i (mp/matrix-add (m i) (mp/matrix-multiply (m j) factor))))))

;;;
;;; math operations instantiated
;;;

(add-template lib
              (extend-protocol mp/PDimensionInfo
                array-wrap-type#
                (dimensionality [m]
                  (if (== 0 (.length m))
                    1
                    (inc (mp/dimensionality (.nth m 0)))))
                (is-vector? [m]
                  (vector-1d? m))
                (is-scalar? [m]
                  false)
                (get-shape [m]
                  (let [c (.length m)]
                    (cons c (if (> c 0)
                              (mp/get-shape (m 0))
                              nil))))
                (dimension-count [m x]
                  (if (== x 0)
                    (.length m)
                    (mp/dimension-count (m 0) (dec x))))))

(add-template lib
              (extend-protocol mp/PElementCount
                array-wrap-type#
                (element-count [m]
                  (let [c (long (count m))]
                    (if (== c 0)
                      0
                      (* c (mp/element-count (m 0))))))))

;; we need to implement this for all persistent vectors since we need to check all nested components
(add-template lib
              (extend-protocol mp/PConversion
                array-wrap-type#
                (convert-to-nested-vectors [m]
                  (if (is-nested-persistent-vectors? m)
                    m
                    (let [m (mapv-identity-check mp/convert-to-nested-vectors m)]
                      (if (reduce = (map mp/get-shape m))
                        m
                        (error "Can't convert to persistent vector array: inconsistent shape.")))))))

(add-template lib
              (extend-protocol mp/PFunctionalOperations
                array-wrap-type#
                (element-seq [m]
                  (cond
                    (== 0 (count m))
                    '()
                    (> (mp/dimensionality (m 0)) 0)
                    (mapcat mp/element-seq m)
                    :else
                    (map mp/get-0d m)))
                (element-map
                  ([m f]
                   (mapmatrix f m))
                  ([m f a]
                   (mapmatrix f m (mp/broadcast-like m a)))
                  ([m f a more]
                   (apply mapmatrix f m a more)))
                (element-map!
                  ([m f]
                   (doseq [s m]
                     (mp/element-map! s f))
                   m)
                  ([m f a]
                   (dotimes [i (count m)]
                     (mp/element-map! (m i) f (mp/get-major-slice a i)))
                   m)
                  ([m f a more]
                   (dotimes [i (count m)]
                     (apply mp/element-map! (m i) f (mp/get-major-slice a i) (map #(mp/get-major-slice % i) more)))
                   m))
                (element-reduce
                  ([m f]
                   (reduce f (mp/element-seq m)))
                  ([m f init]
                   (reduce f init (mp/element-seq m))))))
