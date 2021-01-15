(ns chisel.conic-sections
  "Conic sections implementations"
  (:require [chisel.curves :as curves]
            [chisel.protocols :as protocols]
            [chisel.coordinates :as c]))

(def ^:const WEIGHT_90 (Math/sin (/ Math/PI 4)))

(defn elliptic-curve
  "B-Spline elliptic curve"
  [center a b & {:keys [section] :or {section :full}}]
  (let [center->a (c/difference a center)
        center->b (c/difference b center)
        knot-vector (cond
                      (= :quarter section) []
                      (= :half section)    [1/2 1/2]
                      (= :full section)    [1/4 1/4 2/4 2/4 3/4 3/4])]
    (curves/clamped-b-spline
     {:control-points (cond-> [b
                               (c/weighted (c/sum center center->a center->b)
                                           WEIGHT_90)
                               a]
                        (or (= :half section)
                            (= :full section))
                        (into [(c/weighted (c/sum center center->a (c/opposite-vector center->b))
                                           WEIGHT_90)
                               (c/sum center (c/opposite-vector center->b))])
                        (= :full section)
                        (into [(c/weighted (c/sum center (c/opposite-vector center->a) (c/opposite-vector center->b))
                                           WEIGHT_90)
                               (c/sum center (c/opposite-vector center->a))
                               (c/weighted (c/sum center (c/opposite-vector center->a) center->b)
                                           WEIGHT_90)
                               b]))
      :knot-vector knot-vector
      :order 2})))

(defn circle-arc-point
  "Given points a, b and degrees, returns tuple of `[c-point c-weight]` for construction of arc."
  [a b degrees & {:keys [counterclockwise?]}]
  (let [alpha        (Math/toRadians (/ (- 180 degrees) 2))
          a->b         (c/difference a b)
          a-b-distance (c/vector-length a->b)
          heigth       (/ a-b-distance (Math/tan alpha) 2)]
    [(protocols/linear-transform
      (c/linear-combination 1/2 a b)
      (c/translate-matrix
       (c/scale-vector (c/orthogonal-vector a->b :counterclockwise? counterclockwise?)
                       heigth)))
     (Math/sin alpha)]))

(defn circle-arc
  "Weighted coordinates for circle arc"
  [a b degrees & {:keys [counterclockwise?]}]
  (if (>= degrees 180)
    (let [half-angle      (/ degrees 2)
          alpha           (Math/toRadians (/ (- 360 degrees) 2))
          a->b            (c/difference a b)
          half-base       (/ (c/vector-length a->b) 2)
          base-c-distance (+ (/ half-base (Math/tan alpha))
                             (/ half-base (Math/sin alpha)))
          c               (protocols/linear-transform
                           (c/linear-combination 1/2 a b)
                           (c/translate-matrix
                            (c/scale-vector (c/orthogonal-vector a->b :counterclockwise? counterclockwise?)
                                            base-c-distance)))]
      (into (circle-arc a c half-angle :counterclockwise? counterclockwise?)
            (circle-arc c b half-angle :counterclockwise? counterclockwise?)))
    (let [[c-point c-weight] (circle-arc-point a b degrees :counterclockwise? counterclockwise?)]
      [[a
        (c/weighted c-point c-weight)
        b]])))

(def torus
  (let [curves [(elliptic-curve (c/v [0 0 0]) (c/v [0 100 0]) (c/v [100 0 0]))
                (elliptic-curve (c/v [0 0 5]) (c/v [0 100 5]) (c/v [100 0 5]))
                (elliptic-curve (c/v [0 0 0]) (c/v [0 125 0]) (c/v [125 0 0]))]]
    (curves/tensor-product-patch #(apply elliptic-curve %) curves)))

(def half-torus
  (let [curves [(curves/composite-bezier-curve
                 [[(c/v [-50 0 0]) (c/v [-50 20 0])]
                  [(c/v [-50 20 0]) (c/weighted (c/v [-50 70 0]) 0.707) (c/v [0 70 0])]
                  [(c/v [0 70 0]) (c/weighted (c/v [50 70 0]) 0.707) (c/v [50 20 0])]
                  [(c/v [50 20 0]) (c/v [50 0 0])]])
                (curves/composite-bezier-curve
                 [[(c/v [-50 0 23]) (c/v [-50 20 23])]
                  [(c/v [-50 20 23]) (c/weighted (c/v [-50 70 23]) 0.707) (c/v [0 70 23])]
                  [(c/v [0 70 23]) (c/weighted (c/v [50 70 23]) 0.707) (c/v [50 20 23])]
                  [(c/v [50 20 23]) (c/v [50 0 23])]])
                (curves/composite-bezier-curve
                 [[(c/v [-73 0 0]) (c/v [-73 20 0])]
                  [(c/v [-73 20 0]) (c/weighted (c/v [-73 93 0]) 0.707) (c/v [0 93 0])]
                  [(c/v [0 93 0]) (c/weighted (c/v [73 93 0]) 0.707) (c/v [73 20 0])]
                  [(c/v [73 20 0]) (c/v [73 0 0])]])]]
    (curves/tensor-product-patch #(apply elliptic-curve %) curves)))
