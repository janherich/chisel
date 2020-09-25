(ns chisel.boats
  "Boat hulls"
  (:require [chisel.protocols :as protocols]
            [chisel.curves :as curves]
            [chisel.conic-sections :as conics]
            [chisel.coordinates :as c]
            [chisel.surface-infills :as surface-infills]
            [chisel.utils :as u]
            [chisel.open-scad :as os]
            [chisel.stl :as stl]
            [chisel.gcode :as gcode]
            [chisel.gcode-layers :as gcode-layers]))

(def surf-ski
  (let [deck-curve         (curves/bezier-curve [(c/v [-80 0 0]) (c/v [70 0 0])])
        right-border-curve (curves/clamped-uniform-b-spline
                            {:control-points [(c/v [-80 0 0])
                                              (c/v [-80 1 0])
                                              (c/v [0 5 0])
                                              (c/v [5 5 0])
                                              (c/v [20 5 0])
                                              (c/v [70 1 0])
                                              (c/v [70 0 0])]
                             :order 3})
        right-bottom-curve (curves/clamped-uniform-b-spline
                            {:control-points [(c/v [-80 0 0])
                                              (c/v [-105 1 4])
                                              (c/v [-80 1 5])
                                              (c/v [-10 5 5])
                                              (c/v [0 5 5])
                                              (c/v [25 5 4])
                                              (c/v [65 2 2])
                                              (c/v [70 1 2])
                                              (c/v [70 0 0])]
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
                                                   (c/v [70 0 250]) (c/v [15 0 400])
                                                   (c/v [15 0 440])]
                                  :knot-vector [1/3 2/3]
                                  :order 3})
        right-border-lift-curve (curves/clamped-b-spline
                                 {:control-points [(c/v [80 0 0]) (c/v [80 4 0]) (c/v [80 5 200])
                                                   (c/v [70 7 250]) (c/v [15 15 400])
                                                   (c/v [15 15 440])]
                                  :knot-vector [1/3 2/3]
                                  :order 3})
        spine-curve             (curves/clamped-b-spline
                                 {:control-points [(c/v [0 0 0]) (c/v [0 4 0]) (c/v [0 10 200])
                                                   (c/v [0 10 250]) (c/v [0 15 400])
                                                   (c/v [0 15 440])]
                                  :knot-vector    [1/3 2/3]
                                  :order          3})
        left-border-curve       (protocols/linear-transform right-border-curve (c/flip-matrix :x))
        left-border-lift-curve  (protocols/linear-transform right-border-lift-curve (c/flip-matrix :x))]
    (curves/clamped-b-spline-patch
     {:control-curves [left-border-curve
                       (with-meta left-border-lift-curve {:weight conics/WEIGHT_90})
                       spine-curve
                       (with-meta right-border-lift-curve {:weight conics/WEIGHT_90})
                       right-border-curve]
      :knot-vector    [1/2 1/2]
      :order          2})))

(def sup-blade-outline
  (protocols/linear-transform sup-blade (c/scale-matrix {:y 0})))

(defn sup-blade-ph [x y range]
  (surface-infills/corrugated-surface
   (curves/cut-patch sup-blade range)
   (curves/cut-patch (protocols/linear-transform sup-blade (c/flip-matrix :y)) range)
   x 200 y 100
   :top-skin? true
   :bottom-skin? true))

(def cross-section
  (let [curve (curves/clamped-b-spline
               {:control-points [(c/v [100 0 0]) (c/v [90 40 0]) (c/v [150 110 0])
                                 (c/v [0 250 0])
                                 (c/v [-150 110 0]) (c/v [-90 40 0]) (c/v [-100 0 0])]
                :knot-vector    [1/10 5/10 9/10]
                :order          3})]
    (curves/bezier-patch [curve (protocols/linear-transform curve (c/translate-matrix [0 0 300]))])))

(def inner-cross-section
  (let [curve (curves/clamped-b-spline
               {:control-points [(c/v [93 0 0]) (c/v [83 40 0]) (c/v [140 110 0])
                                 (c/v [0 230 0])
                                 (c/v [-140 110 0]) (c/v [-83 40 0]) (c/v [-93 0 0])]
                :knot-vector    [1/10 5/10 9/10]
                :order          3})]
    (curves/bezier-patch [curve (protocols/linear-transform curve (c/translate-matrix [0 0 300]))])))

(comment
  (stl/write
   (stl/generate-ascii-solid
    (surface-infills/corrugated-surface
     cross-section inner-cross-section
     24 200 10 200))))

(def hammerhead
  (let [right-border-curve (curves/clamped-b-spline
                            {:control-points [(c/v [-2200 0 0]) (c/v [-2200 10 0])
                                              (c/v [-300 100 0])
                                              (c/v [0 100 0])
                                              (c/v [1000 100 0])
                                              (c/v [2000 20 0]) (c/v [2000 0 0])]
                             :knot-vector    [1/4 2/4 3/4]
                             :order          3})
        right-waist-curve  (curves/clamped-b-spline
                            {:control-points [(c/v [-2200 0 40]) (c/v [-2200 10 40])
                                              (c/v [-300 90 40])
                                              (c/v [0 90 40])
                                              (c/v [1000 90 40])
                                              (c/v [2000 20 40]) (c/v [2000 0 40])]
                             :knot-vector    [1/4 2/4 3/4]
                             :order          3})
        right-bottom-curve (curves/clamped-b-spline
                            {:control-points [(c/v [-2200 0 110]) (c/v [-2200 10 110])
                                              (c/v [-300 150 110])
                                              (c/v [0 150 110])
                                              (c/v [1000 150 110])
                                              (c/v [2000 20 110]) (c/v [2000 0 110])]
                             :knot-vector    [1/4 2/4 3/4]
                             :order          3})
        keel-curve         (curves/clamped-b-spline
                            {:control-points [(c/v [-2200 0 220]) (c/v [-2200 0 220])
                                              (c/v [-300 0 250])
                                              (c/v [0 0 250])
                                              (c/v [1000 0 250])
                                              (c/v [2000 0 220]) (c/v [2000 0 220])]
                             :knot-vector    [1/4 2/4 3/4]
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

(def hammerhead-inside
  (protocols/linear-transform hammerhead (c/scale-matrix {;;:x (/ 4200 4180)
                                                          :y (/ 300 280)
                                                          :z (/ 250 230)})))

(defn hammerhead-section [interval x-corrugations y-corrugations]
  (surface-infills/corrugated-surface
   (curves/cut-patch hammerhead interval)
   (curves/cut-patch hammerhead-inside interval)
   x-corrugations 200 y-corrugations 200))

(def small-boat
  (let [right-border-curve (curves/clamped-uniform-b-spline
                            {:control-points [(c/v [0 0 -1000]) (c/v [0 120 -500])
                                              (c/v [0 200 0])
                                              (c/v [0 120 500]) (c/v [0 0 1000])]
                             :order          3})
        right-bottom-curve (protocols/linear-transform right-border-curve (c/translate-matrix [100 0 0]))
        keel-curve         (curves/clamped-uniform-b-spline
                            {:control-points [(c/v [100 0 -1000]) (c/v [160 0 -500])
                                              (c/v [160 0 0])
                                              (c/v [160 0 500]) (c/v [100 0 1000])]
                             :order          3})
        left-border-curve  (protocols/linear-transform right-border-curve (c/flip-matrix :y))
        left-bottom-curve (protocols/linear-transform left-border-curve (c/translate-matrix [100 0 0]))]
    (curves/clamped-uniform-b-spline-patch
     {:control-curves [(curves/axis-uniform-curve (protocols/polyline left-border-curve 2000) :z)
                       (curves/axis-uniform-curve (protocols/polyline left-bottom-curve 2000) :z)
                       (curves/axis-uniform-curve (protocols/polyline keel-curve 2000) :z)
                       (curves/axis-uniform-curve (protocols/polyline right-bottom-curve 2000) :z)
                       (curves/axis-uniform-curve (protocols/polyline right-border-curve 2000) :z)]
      :order          2})))

(def small-boat-inside
  (let [right-border-curve (curves/clamped-uniform-b-spline
                            {:control-points [(c/v [0 0 -1000]) (c/v [0 110 -500])
                                              (c/v [0 180 0])
                                              (c/v [0 110 500]) (c/v [0 0 1000])]
                             :order          3})
        right-bottom-curve (protocols/linear-transform right-border-curve (c/translate-matrix [100 0 0]))
        keel-curve         (curves/clamped-uniform-b-spline
                            {:control-points [(c/v [100 0 -1000]) (c/v [140 0 -500])
                                              (c/v [140 0 0])
                                              (c/v [140 0 500]) (c/v [100 0 1000])]
                             :order          3})
        left-border-curve  (protocols/linear-transform right-border-curve (c/flip-matrix :y))
        left-bottom-curve (protocols/linear-transform left-border-curve (c/translate-matrix [100 0 0]))]
    (curves/clamped-uniform-b-spline-patch
     {:control-curves [(curves/axis-uniform-curve (protocols/polyline left-border-curve 2000) :z)
                       (curves/axis-uniform-curve (protocols/polyline left-bottom-curve 2000) :z)
                       (curves/axis-uniform-curve (protocols/polyline keel-curve 2000) :z)
                       (curves/axis-uniform-curve (protocols/polyline right-bottom-curve 2000) :z)
                       (curves/axis-uniform-curve (protocols/polyline right-border-curve 2000) :z)]
      :order          2})))

(defn small-boat-section
  [interval-1 interval-2 layers resolution corrugations]
  (let [top    (protocols/linear-transform (curves/cut-patch small-boat interval-1 interval-2) (c/scale-matrix 1/2))
        bottom (protocols/linear-transform (curves/cut-patch small-boat-inside interval-1 interval-2) (c/scale-matrix 1/2))]
    #_(u/triangle-mesh-surface (protocols/triangle-mesh top [100 100]))
    (gcode-layers/corrugated-panel-descriptor top bottom 20 layers resolution)
    #_(os/write
       (os/generate-polyhedron
        (protocols/triangle-mesh top [100 100]))
       (os/generate-polyhedron
        (protocols/triangle-mesh bottom [100 100])))))
