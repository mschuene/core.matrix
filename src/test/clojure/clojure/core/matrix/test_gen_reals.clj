(ns clojure.core.matrix.test-gen-reals
  (:use clojure.test)
  (:require  [clojure.core.matrix :as m])
  (:require [clojure.core.matrix.protocols :as mp])
  (:use clojure.core.matrix.utils)
  (:require [clojure.core.matrix.operators :as op])
  (:require [clojure.core.matrix.impl.wrappers :as wrap])
  (:require [clojure.core.matrix.compliance-tester])
  (:require [clojure.core.matrix.impl.stag-pv-doubles :as spvd])
  (:require [clojure.core.matrix.impl.wrap-pv-doubles :as wpvd])
  (:require [clojure.core.matrix.impl.wrap2-pv-doubles :as wpvd2])
  (:refer-clojure :exclude [vector?]))

(def v (m/matrix [1 2 3]))
(def u (m/matrix [[2] [3] [4]]))

;; see pv_reals
;; instantiates pv-generic with sc-add == *, sc-mul == *!!
;; quick example
(m/mmul v u)

(def XS [[1 2][3 4]])
(def YS [1 2])

(println "PV direct")
(m/mmul XS YS)
(time (dotimes [_ 1000000] (m/mmul XS YS)))
(time (dotimes [_ 1000000] (m/mmul XS YS)))
(time (dotimes [_ 1000000] (m/mmul XS YS)))
(time (dotimes [_ 1000000] (m/mmul XS YS)))

(def XW (wpvd/pv-wrap [[1 2][3 4]]))
(def YW (wpvd/pv-wrap [1 2]))

(println "PV wrapped")
(m/mmul XW YW)
(time (dotimes [_ 1000000] (m/mmul XW YW)))
(time (dotimes [_ 1000000] (m/mmul XW YW)))
(time (dotimes [_ 1000000] (m/mmul XW YW)))
(time (dotimes [_ 1000000] (m/mmul XW YW)))


(def XW2 (wpvd2/pv-wrap [[1 2][3 4]]))
(def YW2 (wpvd2/pv-wrap [1 2]))

(class XW2)
(m/mmul XW2 YW2)
