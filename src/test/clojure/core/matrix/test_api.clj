(ns core.matrix.test-api
  (:use clojure.test)
  (:use core.matrix)
  (:require [core.matrix.protocols :as mp])
  (:require [core.matrix.operators :as op])
  (:require core.matrix.examples)
  (:require core.matrix.impl.persistent-vector)
  (:refer-clojure :exclude [vector?]))

(deftest test-indexed-access
  (testing "clojure vector indexed access"
    (is (== 1 (mget [1 2 3] 0)))
    (is (== 1 (mget [[1 2 3] [4 5 6]] 0 0)))
    (is (== 8 (mget [[[1 2] [3 4]] [[5 6] [7 8]]] 1 1 1)))))

(deftest test-coerce
  (testing "clojure vector coercion"
    (is (== 1.0 (coerce [] 1)))
    (is (= [1 2 3] (coerce [[1 0 0] [0 1 0] [0 0 1]] [1 2 3])))
    (is (= [[1 2] [3 4]] (coerce [1] [[1 2] [3 4]])))
    (is (= [[1 2] [3 4]] (coerce [1] '((1 2) (3 4)))))))
(deftest test-slices
  (testing "rows and columns of clojure vector matrix"
    (is (= [1 2 3] (get-row [[1 2 3] [4 5 6]] 0)))
    (is (= [2 5] (get-column [[1 2 3] [4 5 6]] 1))))
  (testing "get-nd on scalar with zero dimensions"
    (is (== 10.0 (mget 10.0)))
    (is (== 10.0 (mp/get-nd 10.0 []))))
  (testing "slices clojure vector"
    (is (= [1 2 3] (slices [1 2 3])))))

(deftest test-functional-ops
  (testing "eseq"
    (is (= [1] (eseq 1)))
    (is (= [1] (eseq [1])))
    (is (= [1] (eseq [[1]])))
    (is (= [1] (eseq [[[1]]])))))

(deftest test-equals
  (testing "scalars"
    (is (equals 6 6.0))
    (is (equals 6 (mul 3 2)))
    (is (equals 6.0 (scale 3 2))))
  (testing "vectors"
    (is (not (equals [6] [3])))
    (is (equals [6] [6.0]))
    (is (equals [6] (mul [3] 2)))
    (is (equals [6.0] (mul 2 [3]))))
  (testing "matrices"
    (is (equals [[1 0.0] [0 1.0]] [[1.0 0] [0.0 1]]))))

(deftest test-multiply
  (testing "scalars"
    (is (== 6 (mul 3 2)))
    (is (== 6 (scale 3 2)))
    (is (== 6 (mp/pre-scale 3 2))))
  (testing "matrix scaling"
    (is (= [6.0] (mul [3] 2)))
    (is (= [6.0] (mul 2 [3])))
    (is (= [[6.0]] (mul 2 [[3]])))
    (is (= [[6.0]] (mul [[2]] 3)))))

(deftest test-addition
  (testing "matrix addition"
    (is (= [5.0] (add [3.0] [2.0])))
    (is (= [[6.0]] (add [[2.0]] [[4.0]])))
    (is (= [[[6.0]]] (add [[[2.0]]] [[[4.0]]])))))

(deftest test-subtraction
  (testing "matrix subtraction"
    (is (= [1.0] (sub [3.0] [2.0])))
    (is (= [[8.0]] (sub [[12.0]] [[4.0]])))
    (is (= [[[8.0]]] (sub [[[12.0]]] [[[4.0]]])))))

(deftest test-transpose
  (testing "transpose different dimensionalities"
    (is (= 1 (transpose 1)))
    (is (= [1.0] (transpose [1.0])))
    (is (= [[1 3] [2 4]] (transpose [[1 2] [3 4]])))
    (is (= [[1] [2] [3]] (transpose [[1 2 3]])))))

(deftest test-normalise
  (testing "vector normalise"
    (is (= [1.0] (normalise [1.0])))
    (is (= [1.0] (normalise [2.0])))
    (is (= [-1.0 0.0] (normalise [-2.0 0.0])))))

(deftest test-mathsops
  (testing "ops on scalars"
    (is (== 1.0 (floor 1.2)))
    (is (thrown? Throwable (floor! 1.2))))
  (testing "ops"
    (is (= [1.0 2.0] (floor [1.2 2.7]))))) 

(deftest test-scalar
  (testing "numbers as scalars"
    (is (scalar? 1))
    (is (scalar? 1.0))
    (is (scalar? 1/7)))
  (testing "scalar dimensionality"
    (is (== 0 (dimensionality 1.0)))
    (is (== 1.0 (mget 1.0)))
    (is (= [] (shape 1.0))))
  (testing "functional operations"
    (is (= 2.0 (emap inc 1.0)))
    (is (= 10.0 (emap + 4.0 6.0)))
    (is (= 10.0 (emap + 1.0 2.0 3.0 4.0)))
    (is (== 10.0 (ereduce #(+ %1 %2) 10.0)))
    (is (== 3.0 (ereduce + 1.0 2.0)))
    (is (= [1.0] (eseq 1.0)))))

(deftest test-vector-ops
  (testing "vector dot product"
    (is (== 1.0 (dot [1.0] [1.0])))
    (is (== -1.0 (dot [1 2] [1 -1])))))

(deftest test-dimensions
  (testing "vector dimensions"
    (is (= 3 (row-count [1 2 3])))
    (is (= 3 (row-count [[1 2] [2 3] [3 4]])))
    (is (= 2 (column-count [[1 2] [2 3] [3 4]])))
    (is (= [3 2] (shape [[1 2] [2 3] [3 4]])))
    (is (= [2 2 2] (shape [[[1 2] [2 3]] [[3 4] [5 6]]])))))

(deftest check-examples
  (testing "example code"
    (core.matrix.examples/all-examples)))

(deftest test-predicates
  (testing "clojure vector predicates"
    (is (array? [1 2]))
    (is (vec? [1 2]))
    (is (array? [[1 2] [3 4]]))
    (is (matrix? [[1 2] [3 4]]))
    (is (not (vec? [[1 2] [3 4]])))
    (is (not (matrix? [[[1 2] [2 3]] [[3 4] [5 6]]]))))
  (testing "row and column predicates"
    (is (not (column-matrix? [1])))
    (is (column-matrix? [[1]]))
    (is (not (row-matrix? [1])))
    (is (row-matrix? [[1]]))
    (is (not (column-matrix? [1 2])))
    (is (column-matrix? [[1] [2]]))
    (is (not (column-matrix? [[1 2 3]])))
    (is (row-matrix? [[1 2 3]]))
    (is (not (row-matrix? [1 2]))))
  (testing "mutability"
    (is (not (mutable? [1 2])))
    (is (mutable? (double-array [1 2])))))
