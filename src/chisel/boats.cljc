(ns chisel.boats
  "Boat hulls"
  (:require [chisel.protocols :as protocols]
            [chisel.curves :as curves]
            [chisel.conic-sections :as conics]
            [chisel.coordinates :as c]
            [chisel.surface-infills :as surface-infills]
            [chisel.utils :as u]
            [chisel.open-scad :as os]
            [chisel.stl :as stl]))

(def surf-ski
  (let [deck-curve         (curves/bezier-curve [(c/v [-80 0 0]) (c/v [70 0 0])])
        right-border-curve (curves/clamped-b-spline
                            {:control-points [(c/v [-80 0 0]) (c/v [-80 1 0]) (c/v [0 5 0])
                                              (c/v [5 5 0]) (c/v [20 5 0])
                                              (c/v [70 1 0]) (c/v [70 0 0])]
                             :knot-vector [1/4 2/4 3/4]
                             :order 3})
        right-bottom-curve (curves/clamped-b-spline
                            {:control-points [(c/v [-80 0 0]) (c/v [-105 1 4]) (c/v [-80 1 5])
                                              (c/v [-10 5 5]) (c/v [0 5 5])
                                              (c/v [25 5 4]) (c/v [65 2 2])
                                              (c/v [70 1 2]) (c/v [70 0 0])]
                             :knot-vector [1/6 2/6 3/6 4/6 5/6]
                             :order 3})
        left-border-curve  (protocols/linear-transform right-border-curve (c/flip-matrix :y))
        left-bottom-curve  (protocols/linear-transform right-bottom-curve (c/flip-matrix :y))]
    (curves/clamped-b-spline-patch
     {:control-curves [deck-curve
                       left-border-curve left-bottom-curve
                       right-bottom-curve right-border-curve
                       deck-curve]
      :knot-vector     [1/20 19/20]
      :order           3})))

(def sup-blade
  (let [right-border-curve      (curves/clamped-b-spline
                                 {:control-points [(c/v [80 0 0]) (c/v [80 0 0]) (c/v [80 0 200])
                                                   (c/v [70 0 250]) (c/v [10 0 340])
                                                   (c/v [10 0 355])]
                                  :knot-vector [1/3 2/3]
                                  :order 3})
        right-border-lift-curve (curves/clamped-b-spline
                                 {:control-points [(c/v [80 0 0]) (c/v [80 4 0]) (c/v [80 5 200])
                                                   (c/v [70 7 250]) (c/v [10 10 340])
                                                   (c/v [10 10 355])]
                                  :knot-vector [1/3 2/3]
                                  :order 3})
        spine-curve             (curves/clamped-b-spline
                                 {:control-points [(c/v [0 0 0]) (c/v [0 4 0]) (c/v [0 10 200])
                                                   (c/v [0 10 250]) (c/v [0 10 340])
                                                   (c/v [0 10 355])]
                                  :knot-vector    [1/3 2/3]
                                  :order          3})
        left-border-curve       (protocols/linear-transform right-border-curve (c/flip-matrix :x))
        left-border-lift-curve  (protocols/linear-transform right-border-lift-curve (c/flip-matrix :x))]
    (curves/clamped-b-spline-patch
     {:control-curves [left-border-curve left-border-lift-curve
                       spine-curve
                       right-border-lift-curve right-border-curve]
      :knot-vector    [1/3 2/3]
      :order          2})))

(defn sup-blade-ph [x y]
  (surface-infills/corrugated-surface
   sup-blade
   (protocols/linear-transform sup-blade (c/flip-matrix :y))
   x 100 y 100))

(def cross-section
  (let [curve (curves/clamped-b-spline
               {:control-points [(c/v [100 0 0]) (c/v [90 40 0]) (c/v [150 110 0])
                                 (c/v [0 220 0])
                                 (c/v [-150 110 0]) (c/v [-90 40 0]) (c/v [-100 0 0])]
                :knot-vector    [1/10 5/10 9/10]
                :order          3})]
    (curves/bezier-patch [curve (protocols/linear-transform curve (c/translate-matrix [0 0 300]))])))

(def hammerhead
  (let [right-border-curve (curves/clamped-b-spline
                            {:control-points [(c/v [-2000 0 0]) (c/v [-2000 10 0]) (c/v [-500 100 0])
                                              (c/v [0 100 0])
                                              (c/v [2000 100 0]) (c/v [2000 0 0])]
                             :knot-vector    [1/3 2/3]
                             :order          3})
        right-waist-curve  (curves/clamped-b-spline
                            {:control-points [(c/v [-2000 0 40]) (c/v [-2000 10 40]) (c/v [-500 90 40])
                                              (c/v [0 90 40])
                                              (c/v [2000 90 40]) (c/v [2000 0 40])]
                             :knot-vector    [1/3 2/3]
                             :order          3})
        right-bottom-curve (curves/clamped-b-spline
                            {:control-points [(c/v [-2000 0 110]) (c/v [-2000 10 110]) (c/v [-500 150 110])
                                              (c/v [0 150 110])
                                              (c/v [2000 150 110]) (c/v [2000 0 110])]
                             :knot-vector    [1/3 2/3]
                             :order          3})
        keel-curve         (curves/clamped-b-spline
                            {:control-points [(c/v [-2000 0 220]) (c/v [-2000 0 220]) (c/v [-500 0 220])
                                              (c/v [0 0 220])
                                              (c/v [2000 0 220]) (c/v [2000 0 220])]
                             :knot-vector    [1/3 2/3]
                             :order          3})
        left-border-curve  (protocols/linear-transform right-border-curve (c/flip-matrix :y))
        left-waist-curve   (protocols/linear-transform right-waist-curve (c/flip-matrix :y))
        left-bottom-curve  (protocols/linear-transform right-bottom-curve (c/flip-matrix :y))]
    (curves/clamped-b-spline-patch
     {:control-curves [left-border-curve left-waist-curve left-bottom-curve
                       keel-curve
                       right-bottom-curve right-waist-curve right-border-curve]
      :knot-vector    [1/10 5/10 9/10]
      :order          3})))
