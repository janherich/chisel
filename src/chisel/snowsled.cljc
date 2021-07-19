(ns chisel.snowsled
  "Snowsled"
  (:require [chisel.protocols :as protocols]
            [chisel.curves :as curves]
            [chisel.coordinates :as c]
            [chisel.utils :as u]
            [chisel.open-scad :as os]
            [chisel.gcode :as gcode]
            [chisel.gcode-layers :as gcode-layers]))

(def ^:private length-cm 80)

(def snowsled-upper-face
  (let [overall-length (* 10 length-cm)
        right-curve-1  (curves/clamped-uniform-b-spline
                        {:control-points [(c/v [200 -5 0])
                                          (c/v [200 -5 500])
                                          (c/v [200 -5 650])
                                          (c/v [150 75 overall-length])]
                         :order          2})
        right-curve-2  (curves/clamped-uniform-b-spline
                        {:control-points [(c/v [200 0 0])
                                          (c/v [200 0 500])
                                          (c/v [200 0 650])
                                          (c/v [150 80 overall-length])]
                         :order          2})
        right-curve-3  (curves/clamped-uniform-b-spline
                        {:control-points [(c/v [190 0 0])
                                          (c/v [190 0 500])
                                          (c/v [190 0 650])
                                          (c/v [140 80 overall-length])]
                         :order          2})
        right-curve-4  (curves/clamped-uniform-b-spline
                        {:control-points [(c/v [120 0 0])
                                          (c/v [120 0 500])
                                          (c/v [120 0 650])
                                          (c/v [120 80 overall-length])]
                         :order          2})
        right-curve-5  (curves/clamped-uniform-b-spline
                        {:control-points [(c/v [100 0 0])
                                          (c/v [100 0 500])
                                          (c/v [100 0 650])
                                          (c/v [100 80 overall-length])]
                         :order          2})
        right-curve-6  (curves/clamped-uniform-b-spline
                        {:control-points [(c/v [100 20 0])
                                          (c/v [100 20 500])
                                          (c/v [100 90 overall-length])]
                         :order          2})
        right-curve-7  (curves/clamped-uniform-b-spline
                        {:control-points [(c/v [100 80 0])
                                          (c/v [100 50 50])
                                          (c/v [100 50 200])
                                          (c/v [100 80 500])
                                          (c/v [100 90 overall-length])]
                         :order          2})
        right-curve-8  (curves/clamped-uniform-b-spline
                        {:control-points [(c/v [100 100 0])
                                          (c/v [100 70 50])
                                          (c/v [100 70 200])
                                          (c/v [100 100 500])
                                          (c/v [100 100 overall-length])]
                         :order          2})
        right-curve-9  (curves/clamped-uniform-b-spline
                        {:control-points [(c/v [80 120 0])
                                          (c/v [80 90 50])
                                          (c/v [80 90 200])
                                          (c/v [80 120 300])
                                          (c/v [80 120 500])
                                          (c/v [80 100 overall-length])]
                         :order          2})
        left-curve-1   (protocols/linear-transform right-curve-1 (c/flip-matrix :x))
        left-curve-2   (protocols/linear-transform right-curve-2 (c/flip-matrix :x))
        left-curve-3   (protocols/linear-transform right-curve-3 (c/flip-matrix :x))
        left-curve-4   (protocols/linear-transform right-curve-4 (c/flip-matrix :x))
        left-curve-5   (protocols/linear-transform right-curve-5 (c/flip-matrix :x))
        left-curve-6   (protocols/linear-transform right-curve-6 (c/flip-matrix :x))
        left-curve-7   (protocols/linear-transform right-curve-7 (c/flip-matrix :x))
        left-curve-8   (protocols/linear-transform right-curve-8 (c/flip-matrix :x))
        left-curve-9   (protocols/linear-transform right-curve-9 (c/flip-matrix :x))]
    (curves/clamped-uniform-b-spline-patch
     {:control-curves [(curves/unify-curve right-curve-1 overall-length)
                       (curves/unify-curve right-curve-2 overall-length)
                       (curves/unify-curve right-curve-3 overall-length)
                       (curves/unify-curve right-curve-4 overall-length)
                       (curves/unify-curve right-curve-5 overall-length)
                       (curves/unify-curve right-curve-6 overall-length)
                       (curves/unify-curve right-curve-7 overall-length)
                       (curves/unify-curve right-curve-8 overall-length)
                       (curves/unify-curve right-curve-9 overall-length)
                       (curves/unify-curve left-curve-9 overall-length)
                       (curves/unify-curve left-curve-8 overall-length)
                       (curves/unify-curve left-curve-7 overall-length)
                       (curves/unify-curve left-curve-6 overall-length)
                       (curves/unify-curve left-curve-5 overall-length)
                       (curves/unify-curve left-curve-4 overall-length)
                       (curves/unify-curve left-curve-3 overall-length)
                       (curves/unify-curve left-curve-2 overall-length)
                       (curves/unify-curve left-curve-1 overall-length)]
      :order           2})))

(def snowsled-lower-face
  (let [overall-length (* 10 length-cm)
        right-curve-1  (curves/clamped-uniform-b-spline
                        {:control-points [(c/v [200 -5 0])
                                          (c/v [200 -5 500])
                                          (c/v [200 -5 650])
                                          (c/v [150 75 overall-length])]
                         :order          2})
        right-curve-2  (curves/clamped-uniform-b-spline
                        {:control-points [(c/v [200 -10 0])
                                          (c/v [200 -10 500])
                                          (c/v [200 -10 650])
                                          (c/v [150 70 overall-length])]
                         :order          2})
        right-curve-3  (curves/clamped-uniform-b-spline
                        {:control-points [(c/v [190 -10 0])
                                          (c/v [190 -10 500])
                                          (c/v [190 -10 650])
                                          (c/v [140 70 overall-length])]
                         :order          2})
        right-curve-4  (curves/clamped-uniform-b-spline
                        {:control-points [(c/v [120 -10 0])
                                          (c/v [120 -10 500])
                                          (c/v [120 -10 650])
                                          (c/v [120 70 overall-length])]
                         :order          2})
        right-curve-5  (curves/clamped-uniform-b-spline
                        {:control-points [(c/v [90 -10 0])
                                          (c/v [90 -10 500])
                                          (c/v [90 -10 650])
                                          (c/v [90 70 overall-length])]
                         :order          2})
        right-curve-6  (curves/clamped-uniform-b-spline
                        {:control-points [(c/v [90 20 0])
                                          (c/v [90 20 500])
                                          (c/v [90 80 overall-length])]
                         :order          2})
        right-curve-7  (curves/clamped-uniform-b-spline
                        {:control-points [(c/v [90 70 0])
                                          (c/v [90 40 50])
                                          (c/v [90 40 200])
                                          (c/v [90 70 500])
                                          (c/v [90 80 overall-length])]
                         :order          2})
        right-curve-8  (curves/clamped-uniform-b-spline
                        {:control-points [(c/v [90 90 0])
                                          (c/v [90 60 50])
                                          (c/v [90 60 200])
                                          (c/v [90 90 500])
                                          (c/v [90 90 overall-length])]
                         :order          2})
        right-curve-9  (curves/clamped-uniform-b-spline
                        {:control-points [(c/v [80 110 0])
                                          (c/v [80 80 50])
                                          (c/v [80 80 200])
                                          (c/v [80 110 300])
                                          (c/v [80 110 500])
                                          (c/v [80 90 overall-length])]
                         :order          2})
        left-curve-1   (protocols/linear-transform right-curve-1 (c/flip-matrix :x))
        left-curve-2   (protocols/linear-transform right-curve-2 (c/flip-matrix :x))
        left-curve-3   (protocols/linear-transform right-curve-3 (c/flip-matrix :x))
        left-curve-4   (protocols/linear-transform right-curve-4 (c/flip-matrix :x))
        left-curve-5   (protocols/linear-transform right-curve-5 (c/flip-matrix :x))
        left-curve-6   (protocols/linear-transform right-curve-6 (c/flip-matrix :x))
        left-curve-7   (protocols/linear-transform right-curve-7 (c/flip-matrix :x))
        left-curve-8   (protocols/linear-transform right-curve-8 (c/flip-matrix :x))
        left-curve-9   (protocols/linear-transform right-curve-9 (c/flip-matrix :x))]
    (curves/clamped-uniform-b-spline-patch
     {:control-curves [(curves/unify-curve right-curve-1 overall-length)
                       (curves/unify-curve right-curve-2 overall-length)
                       (curves/unify-curve right-curve-3 overall-length)
                       (curves/unify-curve right-curve-4 overall-length)
                       (curves/unify-curve right-curve-5 overall-length)
                       (curves/unify-curve right-curve-6 overall-length)
                       (curves/unify-curve right-curve-7 overall-length)
                       (curves/unify-curve right-curve-8 overall-length)
                       (curves/unify-curve right-curve-9 overall-length)
                       (curves/unify-curve left-curve-9 overall-length)
                       (curves/unify-curve left-curve-8 overall-length)
                       (curves/unify-curve left-curve-7 overall-length)
                       (curves/unify-curve left-curve-6 overall-length)
                       (curves/unify-curve left-curve-5 overall-length)
                       (curves/unify-curve left-curve-4 overall-length)
                       (curves/unify-curve left-curve-3 overall-length)
                       (curves/unify-curve left-curve-2 overall-length)
                       (curves/unify-curve left-curve-1 overall-length)]
      :order           2})))

(defn render []
  (os/write
   (os/generate-polyhedron
    (protocols/triangle-mesh snowsled-upper-face [100 100]))
   (os/generate-polyhedron
    (protocols/triangle-mesh snowsled-lower-face [100 100]))))

(def ^:private layers-per-cm 50)

(defn snowsled []
  (merge (gcode-layers/corrugated-panel-descriptor snowsled-upper-face
                                                   snowsled-lower-face
                                                   35
                                                   (* layers-per-cm length-cm)
                                                   200
                                                   :corrugate-fn (partial gcode-layers/cutoff-corrugations 1/50)
                                                   :modulate-curve (gcode-layers/sine-curve 1/120 20))
         {:skirt-polyline (gcode/circular-polyline 210 0)}))

(comment
  (u/write-to-file
   "/Users/janherich/CAD/Snowsled.gcode"
   (gcode/generate-gcode (merge gcode/bigprinter-print-descriptor (snowsled)))))
