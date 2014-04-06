(ns clojure.core.matrix.test-gen-reals
  (:use clojure.test)
  (:require  [clojure.core.matrix :as m])
  (:use clojure.core.matrix.utils)
  (:require [clojure.core.matrix.operators :as op])
  (:require [clojure.core.matrix.impl.wrappers :as wrap])
  (:require [clojure.core.matrix.compliance-tester])
  (:require clojure.core.matrix.impl.stag-pv-doubles)
  (:refer-clojure :exclude [vector?]))

(def v (m/matrix [1 2 3]))
(def u (m/matrix [[2] [3] [4]]))

;; see pv_reals
;; instantiates pv-generic with sc-add == *, sc-mul == *!!
;; quick example
(m/mmul v u)

(m/pm (m/mmul [[1 2] [3 4]] [1 2]))
(time (dotimes [_ 1000000] (m/mmul [[1 2] [3 4]] [1 2])))

(println (m/current-implementation))

(println "XXX")
