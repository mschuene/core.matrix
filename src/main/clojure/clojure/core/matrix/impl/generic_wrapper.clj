(ns clojure.core.matrix.impl.generic-wrapper
  (:require [clojure.core.matrix.protocols :as mp]
            [clojure.core.matrix.generic-protocols :as gmp]))


(deftype GenericWrapper [container spec]
  Object
  (toString [gen] (str (:name spec) " " container))
  clojure.lang.Seqable
  (seq [gen] (seq container))
  mp/PImplementation
  (implementation-key [gen] :generic-wrapper)
  (meta-info [gen]
    {:doc "Wraps a container and a specialisation to make the generic functions available to the clojure.core.matrix api"})
  (new-vector [gen length]
    (GenericWrapper. (mp/new-vector container length) spec))
  (new-matrix [gen rows columns]
    (mp/new-matrix container rows columns))
  (new-matrix-nd [gen dims]
    (mp/new-matrix-nd container dims))
  (construct-matrix [gen data]
    (mp/construct-matrix container data))
  (supports-dimensionality? [gen dims]
    (mp/supports-dimensionality? container dims))
  mp/PDimensionInfo
  (dimensionality [gen]
    (mp/dimensionality container))
  (get-shape [gen]
    (mp/get-shape container))
  (is-scalar? [gen]
    (mp/is-scalar? container))
  (is-vector? [gen]
    (mp/is-vector? gen))
  (dimension-count [gen dimension-number]
    (mp/dimension-count container dimension-number))
  mp/PIndexedAccess
  (get-1d [gen row]
    (mp/get-1d container row))
  (get-2d [gen row column]
    (mp/get-2d container row column))
  (get-nd [gen indexes]
    (mp/get-nd container indexes))
  mp/PIndexedSetting
  (set-1d [gen x v]
    (mp/set-1d container x v))
  (set-2d [gen x y v]
    (mp/set-2d container x y v))
  (set-nd [gen indexes v]
    (mp/set-nd container indexes v))
  (is-mutable? [gen]
    (mp/is-mutable? container))
  mp/PIndexedSettingMutable
  (set-1d! [gen row v]
    (mp/set-1d! container row v))
  (set-2d! [gen row column v]
    (mp/set-2d! container row column v))
  (set-nd! [gen indexes v]
    (mp/set-nd! container indexes v))
  mp/PZeroDimensionAccess
  (get-0d [gen]
    (mp/get-0d container))
  (set-0d! [gen value]
    (mp/set-0d! container value))
  mp/PZeroDimensionSet
  (set-0d [gen value]
    (GenericWrapper. (mp/set-0d container value) spec))
  mp/PMatrixCloning
  (clone [gen]
    (GenericWrapper. (mp/clone container) spec))
  mp/PTypeInfo
  ;;TODO where is this used for -should this return a element-type from the spec?
  (element-type [gen]
    (mp/element-type container))
  mp/PCoercion
  (coerce-param [gen param]
    (GenericWrapper. (mp/coerce-param container param) spec))
  mp/PMutableMatrixConstruction
  (mutable-matrix [gen] (GenericWrapper. (mp/mutable-matrix container) spec))
  mp/PSparse
  (sparse-coerce [gen data]
    (GenericWrapper. (mp/sparse-coerce container data) spec))
  (sparse [gen]
    (GenericWrapper. (mp/sparse container) spec))
  mp/PDense
  (dense-coerce [gen data]
    (GenericWrapper. (mp/dense-coerce container data) spec))
  (dense [gen]
    (GenericWrapper. (mp/dense container) spec))
  mp/PImmutableMatrixConstruction
  (immutable-matrix [gen]
    (GenericWrapper. (mp/immutable-matrix container) spec))
  ;;Numeric protocols
  mp/PMatrixMultiply 
  (matrix-multiply [gen a]
    (GenericWrapper. (gmp/generic-matrix-multiply container a spec) spec))
  (element-multiply [gen a]
    (GenericWrapper. (gmp/generic-element-multiply container a spec) spec))
  mp/PMatrixProducts
  (inner-product [gen a]
    (GenericWrapper. (gmp/generic-inner-product container a spec) spec))
  (outer-product [gen a]
    (GenericWrapper. (gmp/generic-outer-product container a spec) spec))
  mp/PAddProduct
  (add-product [gen a b]
    (GenericWrapper. (gmp/generic-add-product container a b spec) spec))
  mp/PAddProductMutable
  (add-product! [gen a b]
    (gmp/generic-add-product! container a b spec))
  mp/PAddScaledProduct
  (add-scaled-product [gen a b factor]
    (GenericWrapper. (gmp/generic-add-scaled-product container a b factor spec) spec))
  mp/PAddScaledProductMutable
  (add-scaled-product! [gen a b factor]
    (gmp/generic-add-scaled-product! container a b factor spec))
  mp/PAddScaled
  (add-scaled [gen a factor]
    (GenericWrapper. (gmp/generic-add-scaled container a factor spec) spec))
  mp/PAddScaledMutable
  (add-scaled! [gen a factor]
    (gmp/generic-add-scaled! container a factor spec))
  mp/PMatrixDivide
  (element-divide [gen]
    (GenericWrapper. (gmp/generic-element-divide container spec) spec))
  (element-divide [gen a]
    (GenericWrapper. (gmp/generic-element-divide container a spec) spec))
  mp/PMatrixDivideMutable
  (element-divide! [gen]
    (gmp/generic-element-divide! container spec))
  (element-divide! [gen a]
    (gmp/generic-element-divide! container a spec))
  mp/PMatrixMultiplyMutable
  (matrix-multiply! [gen a]
    (gmp/generic-matrix-multiply! container a spec))
  (element-multiply! [gen a]
    (gmp/generic-element-multiply! container a spec))
  mp/PVectorTransform
  (vector-transform [gen v]
    (GenericWrapper. (gmp/generic-vector-transform container v spec) spec))
  (vector-transform! [gen v]
    (gmp/generic-vector-transform! container v spec) spec)
  mp/PMatrixScaling
  (scale [gen a]
    (GenericWrapper. (gmp/generic-scale container a spec) spec))
  (pre-scale [gen a]
    (GenericWrapper. (gmp/generic-pre-scale container a spec) spec))
  mp/PMatrixMutableScaling
  (scale! [gen factor]
    (gmp/generic-scale! container factor spec))
  (pre-scale! [gen factor]
    (gmp/generic-pre-scale! container factor spec))
  mp/PMatrixAdd
  (matrix-add [gen a]
    (GenericWrapper. (gmp/generic-matrix-add container a spec) spec))
  (matrix-sub [gen a]
    (GenericWrapper. (gmp/generic-matrix-sub container a spec) spec))
  mp/PMatrixAddMutable
  (matrix-add! [gen a]
    (gmp/generic-matrix-add! container a spec))
  (matrix-sub! [gen a]
    (gmp/generic-matrix-sub! container a spec))
  mp/PVectorOps
  (vector-dot [gen b]
    (GenericWrapper. (gmp/generic-vector-dot container b spec) spec))
  (length [gen]
    (gmp/generic-length container spec))
  (length-squared [gen]
    (gmp/generic-length-squared container spec))
  (normalise [gen]
    (GenericWrapper. (gmp/generic-normalise container spec) spec))
  mp/PVectorCross
  (cross-product [gen b]
    (GenericWrapper. (gmp/generic-cross-product container b spec) spec))
  (cross-product! [gen b]
    (gmp/generic-cross-product! container b spec))
  mp/PVectorDistance
  (distance [gen b]
    (gmp/generic-distance container b spec))
  mp/PMutableVectorOps
  (normalise! [gen]
    (gmp/generic-normalise container spec))
  mp/PMatrixOps
  (trace [gen]
    (gmp/generic-trace container spec))
  mp/PNegation
  (negate [gen]
    (GenericWrapper. (gmp/generic-negate container spec) spec))
  mp/PSummable
  (element-sum [gen]
    (gmp/generic-element-sum container spec))
  mp/PExponent
  (element-pow [gen exponent]
    (GenericWrapper. (gmp/generic-element-pow container exponent spec) spec))
  mp/PSquare
  (square [gen]
    (GenericWrapper. (gmp/generic-square container spec) spec))
  mp/PElementMinMax
  (element-min [gen]
    (gmp/generic-element-min gen spec))
  (element-max [gen]
    (gmp/generic-element-max gen spec)))
  
  

(defn wrap-generic [array spec]
  (if ((:scalar? spec) array)
    array
    (GenericWrapper. array spec)))