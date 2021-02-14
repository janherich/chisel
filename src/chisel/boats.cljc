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
                            {:control-points [(c/v [-2600 0 0]) (c/v [-2600 10 0])
                                              (c/v [-300 100 0])
                                              (c/v [0 100 0])
                                              (c/v [900 100 0])
                                              (c/v [2400 20 0]) (c/v [2400 0 0])]
                             :knot-vector    [1/4 2/4 3/4]
                             :order          3})
        right-waist-curve  (curves/clamped-b-spline
                            {:control-points [(c/v [-2600 0 40]) (c/v [-2600 10 40])
                                              (c/v [-300 90 40])
                                              (c/v [0 90 40])
                                              (c/v [900 90 40])
                                              (c/v [2400 20 40]) (c/v [2400 0 40])]
                             :knot-vector    [1/4 2/4 3/4]
                             :order          3})
        right-bottom-curve (curves/clamped-b-spline
                            {:control-points [(c/v [-2600 0 110]) (c/v [-2600 10 110])
                                              (c/v [-300 150 110])
                                              (c/v [0 150 110])
                                              (c/v [900 150 110])
                                              (c/v [2400 20 110]) (c/v [2400 0 110])]
                             :knot-vector    [1/4 2/4 3/4]
                             :order          3})
        keel-curve         (curves/clamped-b-spline
                            {:control-points [(c/v [-2600 0 220]) (c/v [-2600 0 220])
                                              (c/v [-300 0 250])
                                              (c/v [0 0 250])
                                              (c/v [900 0 250])
                                              (c/v [2400 0 220]) (c/v [2400 0 220])]
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

(comment
  (os/write
   (os/generate-polyhedron
    (protocols/triangle-mesh hammerhead [100 100]))))

(def hammerhead-inside
  (protocols/linear-transform hammerhead (c/scale-matrix {;;:x (/ 4200 4180)
                                                          :y (/ 300 280)
                                                          :z (/ 250 230)})))

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
     {:control-curves [(curves/unify-curve left-border-curve 2000)
                       (curves/unify-curve left-bottom-curve 2000)
                       (curves/unify-curve keel-curve 2000)
                       (curves/unify-curve right-bottom-curve 2000)
                       (curves/unify-curve right-border-curve 2000 )]
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
     {:control-curves [(curves/unify-curve left-border-curve 2000)
                       (curves/unify-curve left-bottom-curve 2000)
                       (curves/unify-curve keel-curve 2000)
                       (curves/unify-curve right-bottom-curve 2000)
                       (curves/unify-curve right-border-curve 2000)]
      :order          2})))

(defn small-boat-section
  [interval-1 interval-2 layers resolution corrugations]
  (let [top    #_(protocols/linear-transform) (curves/cut-patch small-boat interval-1 interval-2) #_(c/scale-matrix 1)
        bottom #_(protocols/linear-transform) (curves/cut-patch small-boat-inside interval-1 interval-2) #_(c/scale-matrix 1)]
    #_(u/triangle-mesh-surface (protocols/triangle-mesh top [100 100]))
    (gcode-layers/ribbed-corrugated-panel-descriptor top
                                                    bottom
                                                    5
                                                    corrugations
                                              layers
                                              resolution
                                              ;;:modulate-curve (gcode-layers/sine-curve 1/120 6)
                                              )
    #_(os/write
       (os/generate-polyhedron
        (protocols/triangle-mesh top [100 100]))
       (os/generate-polyhedron
        (protocols/triangle-mesh bottom [100 100])))))

(comment
  (u/write-to-file
   "/Users/janherich/CAD/small_boat.gcode"
   (gcode/generate-gcode (merge gcode/bigprinter-print-descriptor
                                {:skirt-polyline (gcode/circular-polyline 150)}
                                (small-boat-section [1/2 6/10] [0 1] 500 200 20)))))
