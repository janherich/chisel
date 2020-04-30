(ns chisel.conic-sections
  "Conic sections implementations"
  (:require [chisel.curves :as curves]
            [chisel.protocols :as protocols]
            [chisel.coordinates :as c]))

(defn elliptic-curve
  [center a b]
  (let [diff-a (c/diff-vectors a center)
        diff-b (c/diff-vectors b center)]
    (curves/clamped-b-spline
     {:control-points [b
                       (c/add-vectors center diff-a diff-b)
                       (c/euclidian->homogenous a 2)
                       (c/add-vectors center diff-a (c/opposite-vector diff-b))
                       (c/add-vectors center (c/opposite-vector diff-b))
                       (c/add-vectors center (c/opposite-vector diff-a) (c/opposite-vector diff-b))
                       (c/euclidian->homogenous (c/add-vectors center (c/opposite-vector diff-a)) 2)
                       (c/add-vectors center (c/opposite-vector diff-a) diff-b)
                       b]
      :knot-vector [1/4 1/4 2/4 2/4 3/4 3/4]
      :order 2})))

(def torus
  (let [curves [(elliptic-curve [0 0 0] [0 100 0] [100 0 0])
                (elliptic-curve [0 0 5] [0 100 5] [100 0 5])
                (elliptic-curve [0 0 0] [0 125 0] [125 0 0])]]
    (curves/tensor-product-patch #(apply elliptic-curve %) curves)))

(def half-torus
  (let [curves [(curves/composite-bezier-curve
                 [[[-50 0 0] [-50 20 0]]
                  [[-50 20 0] (c/euclidian->homogenous [-50 70 0] 0.707) [0 70 0]]
                  [[0 70 0] (c/euclidian->homogenous [50 70 0] 0.707) [50 20 0]]
                  [[50 20 0] [50 0 0]]])
                (curves/composite-bezier-curve
                 [[[-50 0 23] [-50 20 23]]
                  [[-50 20 23] (c/euclidian->homogenous [-50 70 23] 0.707) [0 70 23]]
                  [[0 70 23] (c/euclidian->homogenous [50 70 23] 0.707) [50 20 23]]
                  [[50 20 23] [50 0 23]]])
                (curves/composite-bezier-curve
                 [[[-73 0 0] [-73 20 0]]
                  [[-73 20 0] (c/euclidian->homogenous [-73 93 0] 0.707) [0 93 0]]
                  [[0 93 0] (c/euclidian->homogenous [73 93 0] 0.707) [73 20 0]]
                  [[73 20 0] [73 0 0]]])]]
    (curves/tensor-product-patch #(apply elliptic-curve %) curves)))
