(ns chisel.sup-paddle
  "SUP paddle"
  (:require [chisel.protocols :as protocols]
            [chisel.curves :as curves]
            [chisel.conic-sections :as conics]
            [chisel.coordinates :as c]
            [chisel.utils :as u]
            [chisel.open-scad :as os]
            [chisel.gcode :as gcode]
            [chisel.gcode-layers :as gcode-layers]))

(def ^:private length-cm 44)

(def sup-blade
  (let [overall-length          (* 10 length-cm)
        right-border-curve      (curves/clamped-uniform-b-spline
                                 {:control-points [(c/v [0 80 0])
                                                   (c/v [0 80 200])
                                                   (c/v [0 70 250])
                                                   (c/v [0 15 400])
                                                   (c/v [0 15 440])]
                                  :order 3})
        right-border-lift-curve (curves/clamped-uniform-b-spline
                                 {:control-points [(c/v [0 70 0])
                                                   (c/v [4 70 0])
                                                   (c/v [5 70 200])
                                                   (c/v [7 70 250])
                                                   (c/v [15 15 400])
                                                   (c/v [15 15 440])]
                                  :order 3})
        spine-curve             (curves/clamped-uniform-b-spline
                                 {:control-points [(c/v [0 0 0])
                                                   (c/v [4 0 0])
                                                   (c/v [10 0 200])
                                                   (c/v [10 0 250])
                                                   (c/v [15 0 400])
                                                   (c/v [15 0 440])]
                                  :order          3})
        left-border-curve       (protocols/linear-transform right-border-curve (c/flip-matrix :y))
        left-border-lift-curve  (protocols/linear-transform right-border-lift-curve (c/flip-matrix :y))]
    (curves/clamped-b-spline-patch
     {:control-curves [(curves/unify-curve left-border-curve overall-length)
                       (with-meta
                         (curves/unify-curve left-border-lift-curve overall-length)
                         {:weight conics/WEIGHT_90})
                       (curves/unify-curve spine-curve overall-length)
                       (with-meta
                         (curves/unify-curve right-border-lift-curve overall-length)
                         {:weight conics/WEIGHT_90})
                       (curves/unify-curve right-border-curve overall-length)]
      :knot-vector    [1/2 1/2]
      :order          2})))

(def sup-blade-outline
  (protocols/linear-transform sup-blade (c/scale-matrix {:y 0})))

(def ^:private layers-per-cm 40)

(defn blade-top-part []
  (let [face-1 (protocols/linear-transform
                (curves/cut-patch sup-blade [1/2 1])
                (c/translate-matrix [150 140 -220]))
        face-2 (protocols/linear-transform
                (curves/cut-patch sup-blade [1/2 1])
                (c/combine-matrices
                 (c/translate-matrix [150 140 -220])
                 (c/flip-matrix :x)))]
    (merge (gcode-layers/corrugated-panel-descriptor face-1 face-2 20
                                                     (* layers-per-cm 22)
                                                     200)
           {:skirt-polyline [[50 50] [250 50] [250 250] [50 250] [50 50]]})
    #_(os/write
       (os/generate-polyhedron
        (protocols/triangle-mesh face-1 [100 100]))
       (os/generate-polyhedron
        (protocols/triangle-mesh face-2 [100 100])))))

(defn blade-bottom-part []
  (let [face-1 (protocols/linear-transform
                (curves/reverse-patch
                 (curves/cut-patch sup-blade [0 1/2]))
                (c/combine-matrices
                 (c/translate-matrix [150 140 220])
                 (c/flip-matrix :z)))
        face-2 (protocols/linear-transform
                (curves/reverse-patch
                 (curves/cut-patch sup-blade [0 1/2]))
                (c/combine-matrices
                 (c/translate-matrix [150 140 220])
                 (c/flip-matrix :x)
                 (c/flip-matrix :z)))]
    (merge (gcode-layers/corrugated-panel-descriptor face-1 face-2 20
                                                     (* layers-per-cm 22)
                                                     200)
           {:skirt-polyline [[50 50] [250 50] [250 250] [50 250] [50 50]]})
    #_(os/write
       (os/generate-polyhedron
        (protocols/triangle-mesh face-1 [100 100]))
       (os/generate-polyhedron
        (protocols/triangle-mesh face-2 [100 100])))))
