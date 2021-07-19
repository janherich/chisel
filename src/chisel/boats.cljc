(ns chisel.boats
  "Boat hulls"
  (:require [chisel.protocols :as protocols]
            [chisel.curves :as curves]
            [chisel.conic-sections :as conics]
            [chisel.coordinates :as c]
            [chisel.surface-infills :as surface-infills]
            [chisel.utils :as u]
            [chisel.hull-utils :as h-u]
            [chisel.open-scad :as os]
            [chisel.stl :as stl]
            [chisel.gcode :as gcode]
            [chisel.gcode-layers :as gcode-layers]))

(def ^:private hammerhead-length 5500)

(def ^:private hammerhead-mid-point (/ 13 25))

(def ^:private hammerhead-width 200)

(def ^:private hammerhead-depth 250)

(def ^:private hammerhead-parts 5)

(def hammerhead
  (let [half-width         (/ hammerhead-width 2)
        mid-point          (* hammerhead-mid-point hammerhead-length)
        front-narrowing    (- mid-point 300)
        rear-narrowing     (+ mid-point 900)
        right-border-curve (curves/clamped-uniform-b-spline
                            {:control-points [(c/v [5 0 0])
                                              (c/v [half-width 0 front-narrowing])
                                              (c/v [half-width 0 mid-point])
                                              (c/v [half-width 0 rear-narrowing])
                                              (c/v [5 0 hammerhead-length])]
                             :order          3})
        right-waist-curve  (curves/clamped-uniform-b-spline
                            {:control-points [(c/v [5 -40 0])
                                              (c/v [(+ half-width 20) -40 front-narrowing])
                                              (c/v [(+ half-width 20) -40 mid-point])
                                              (c/v [(+ half-width 20) -40 rear-narrowing])
                                              (c/v [5 -40 hammerhead-length])]
                             :order          3})
        right-bottom-curve (curves/clamped-uniform-b-spline
                            {:control-points [(c/v [5 -110 0])
                                              (c/v [(+ half-width 50) -110 front-narrowing])
                                              (c/v [(+ half-width 50) -110 mid-point])
                                              (c/v [(+ half-width 50) -110 rear-narrowing])
                                              (c/v [5 -50 hammerhead-length])]
                             :order          3})
        keel-curve         (curves/clamped-uniform-b-spline
                            {:control-points [(c/v [0 (- (- hammerhead-depth 30)) 0])
                                              (c/v [0 (- hammerhead-depth) front-narrowing])
                                              (c/v [0 (- hammerhead-depth) mid-point])
                                              (c/v [0 (- hammerhead-depth) rear-narrowing])
                                              (c/v [0 (- (- hammerhead-depth 150)) hammerhead-length])]
                             :order          3})
        left-border-curve  (protocols/linear-transform right-border-curve (c/flip-matrix :x))
        left-waist-curve   (protocols/linear-transform right-waist-curve (c/flip-matrix :x))
        left-bottom-curve  (protocols/linear-transform right-bottom-curve (c/flip-matrix :x))]
    (curves/clamped-b-spline-patch
     {:control-curves [(curves/unify-curve left-border-curve hammerhead-length)
                       (curves/unify-curve left-waist-curve hammerhead-length)
                       (curves/unify-curve left-bottom-curve hammerhead-length)
                       (curves/unify-curve keel-curve hammerhead-length)
                       (curves/unify-curve right-bottom-curve hammerhead-length)
                       (curves/unify-curve right-waist-curve hammerhead-length)
                       (curves/unify-curve right-border-curve hammerhead-length)]
      :knot-vector    [1/10 5/10 9/10]
      :order          3})))

(def hammerhead-inside
  (let [half-width         (- (/ hammerhead-width 2) 10)
        depth              (- hammerhead-depth 5)
        mid-point          (* hammerhead-mid-point hammerhead-length)
        front-narrowing    (- mid-point 300)
        rear-narrowing     (+ mid-point 900)
        right-border-curve (curves/clamped-uniform-b-spline
                            {:control-points [(c/v [0 0 0])
                                              (c/v [half-width 0 front-narrowing])
                                              (c/v [half-width 0 mid-point])
                                              (c/v [half-width 0 rear-narrowing])
                                              (c/v [0 0 hammerhead-length])]
                             :order          3})
        right-waist-curve  (curves/clamped-uniform-b-spline
                            {:control-points [(c/v [0 -40 0])
                                              (c/v [(+ half-width 20) -40 front-narrowing])
                                              (c/v [(+ half-width 20) -40 mid-point])
                                              (c/v [(+ half-width 20) -40 rear-narrowing])
                                              (c/v [0 -40 hammerhead-length])]
                             :order          3})
        right-bottom-curve (curves/clamped-uniform-b-spline
                            {:control-points [(c/v [0 -100 0])
                                              (c/v [(+ half-width 50) -100 front-narrowing])
                                              (c/v [(+ half-width 50) -100 mid-point])
                                              (c/v [(+ half-width 50) -100 rear-narrowing])
                                              (c/v [0 -42.5 hammerhead-length])]
                             :order          3})
        keel-curve         (curves/clamped-uniform-b-spline
                            {:control-points [(c/v [0 (- (- depth 30)) 0])
                                              (c/v [0 (- depth) front-narrowing])
                                              (c/v [0 (- depth) mid-point])
                                              (c/v [0 (- depth) rear-narrowing])
                                              (c/v [0 (- (- depth 150)) hammerhead-length])]
                             :order          3})
        left-border-curve  (protocols/linear-transform right-border-curve (c/flip-matrix :x))
        left-waist-curve   (protocols/linear-transform right-waist-curve (c/flip-matrix :x))
        left-bottom-curve  (protocols/linear-transform right-bottom-curve (c/flip-matrix :x))]
    (curves/clamped-b-spline-patch
     {:control-curves [(curves/unify-curve left-border-curve hammerhead-length)
                       (curves/unify-curve left-waist-curve hammerhead-length)
                       (curves/unify-curve left-bottom-curve hammerhead-length)
                       (curves/unify-curve keel-curve hammerhead-length)
                       (curves/unify-curve right-bottom-curve hammerhead-length)
                       (curves/unify-curve right-waist-curve hammerhead-length)
                       (curves/unify-curve right-border-curve hammerhead-length)]
      :knot-vector    [1/10 5/10 9/10]
      :order          3})))

(defn hammerhead-render []
  (os/write
   (os/generate-polyhedron
    (protocols/triangle-mesh hammerhead [200 200]))
   #_(os/generate-polyhedron
    (protocols/triangle-mesh hammerhead-inside [100 100]))))

(comment
  (u/write-to-file
   "/Users/janherich/CAD/hammerhead_1.gcode"
   (gcode/generate-gcode (merge gcode/bigprinter-print-descriptor
                                (gcode-layers/ribbed-corrugated-panel
                                 (curves/patch-part hammerhead hammerhead-parts 0
                                                    :mid-point hammerhead-mid-point)
                                 (curves/patch-part hammerhead-inside hammerhead-parts 0
                                                    :mid-point hammerhead-mid-point)
                                 :corrugation-overlap 0)))))

(def small-boat
  (let [right-border-curve (curves/clamped-uniform-b-spline
                            {:control-points [(c/v [5 0 0]) (c/v [120 0 550])
                                              (c/v [200 0 1100])
                                              (c/v [120 0 1650]) (c/v [5 0 2200])]
                             :order          3})
        right-bottom-curve (protocols/linear-transform right-border-curve (c/translate-matrix [0 -100 0]))
        keel-curve         (curves/clamped-uniform-b-spline
                            {:control-points [(c/v [0 -100 0])
                                              (c/v [0 -160 1100])
                                              (c/v [0 -100 2200])]
                             :order          2})
        left-border-curve  (protocols/linear-transform right-border-curve (c/flip-matrix :x))
        left-bottom-curve (protocols/linear-transform left-border-curve (c/translate-matrix [0 -100 0]))]
    (curves/clamped-uniform-b-spline-patch
     {:control-curves [(curves/unify-curve left-border-curve 2200)
                       (curves/unify-curve left-bottom-curve 2200)
                       (curves/unify-curve keel-curve 2200)
                       (curves/unify-curve right-bottom-curve 2200)
                       (curves/unify-curve right-border-curve 2200)]
      :order          3})))

(def small-boat-inside
  (let [right-border-curve (curves/clamped-uniform-b-spline
                            {:control-points [(c/v [1 0 0]) (c/v [110 0 550])
                                              (c/v [190 0 1100])
                                              (c/v [110 0 1650]) (c/v [1 0 2200])]
                             :order          3})
        right-bottom-curve (protocols/linear-transform right-border-curve (c/translate-matrix [0 -90 0]))
        keel-curve         (curves/clamped-uniform-b-spline
                            {:control-points [(c/v [0 -90 0])
                                              (c/v [0 -150 1100])
                                              (c/v [0 -90 2200])]
                             :order          2})
        left-border-curve  (protocols/linear-transform right-border-curve (c/flip-matrix :x))
        left-bottom-curve (protocols/linear-transform left-border-curve (c/translate-matrix [0 -90 0]))]
    (curves/clamped-uniform-b-spline-patch
     {:control-curves [(curves/unify-curve left-border-curve 2200)
                       (curves/unify-curve left-bottom-curve 2200)
                       (curves/unify-curve keel-curve 2200)
                       (curves/unify-curve right-bottom-curve 2200)
                       (curves/unify-curve right-border-curve 2200)]
      :order          3})))

(defn small-boat-section
  [interval resolution]
  (let [top    #_(protocols/linear-transform) (curves/cut-patch small-boat interval) #_(c/scale-matrix 1)
        bottom #_(protocols/linear-transform) (curves/cut-patch small-boat-inside interval) #_(c/scale-matrix 1)]
    #_(u/triangle-mesh-surface (protocols/triangle-mesh top [100 100]))
    (gcode-layers/corrugated-panel top bottom)
    #_(os/write
       (os/generate-polyhedron
        (protocols/triangle-mesh top [100 100]))
       (os/generate-polyhedron
        (protocols/triangle-mesh bottom [100 100])))))

(comment
  (u/write-to-file
   "/Users/janherich/CAD/small_boat.gcode"
   (gcode/generate-gcode (merge gcode/deltav1-print-descriptor
                                {:skirt-polyline (gcode/circular-polyline 165)}
                                (small-boat-section [8/10 10/10] 200)))))
