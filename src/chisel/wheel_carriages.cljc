(ns chisel.wheel-carriages
  "Wheel carriage implementations"
  (:require [chisel.protocols :as protocols]
            [chisel.curves :as curves]
            [chisel.conic-sections :as conics]
            [chisel.coordinates :as c]
            [chisel.utils :as u]
            [chisel.open-scad :as os]
            [chisel.gcode :as gcode]
            [chisel.gcode-layers :as gcode-layers]))

(def ^:private layers-per-cm 40)

(def ^:private openbeam-width 20)

(def ^:private openbeam-wheel-radius 9.7)

(defn openbeam-plate-carriage
  ([thickness height]
   (openbeam-plate-carriage thickness height 6 10))
  ([thickness height radius face-width]
   (let [wheel-center        (+ (/ openbeam-width 2) openbeam-wheel-radius)
         parallel-face-start (- wheel-center (/ face-width 2))
         parallel-face-end   (+ wheel-center (/ face-width 2))
         ;; inner face curves
         f1-r1-start-curve   (curves/bezier-curve [(c/v [parallel-face-start 0 0])
                                                   (c/v [parallel-face-start 0 height])])
         f1-r1-middle-curve  (curves/bezier-curve [(c/v [parallel-face-start (- radius) 0])
                                                   (c/v [parallel-face-start (- radius) height])])
         f1-r1-end-curve     (curves/bezier-curve [(c/v [(- parallel-face-start radius) (- radius) 0])
                                                   (c/v [(- parallel-face-start radius) (- radius) height])])
         f1-center-curve     (curves/bezier-curve [(c/v [0 (- radius) 0])
                                                   (c/v [0 (- radius) height])])
         ;; outer face curves
         f2-start-curve      (curves/bezier-curve [(c/v [parallel-face-end 0 0])
                                                   (c/v [parallel-face-end 0 height])])
         f2-r1-start-curve   (curves/bezier-curve [(c/v [parallel-face-end (- thickness) 0])
                                                   (c/v [parallel-face-end (- thickness) height])])
         f2-r1-middle-curve  (curves/bezier-curve [(c/v [parallel-face-end (- (+ thickness radius)) 0])
                                                   (c/v [parallel-face-end (- (+ thickness radius)) height])])
         f2-r1-end-curve     (curves/bezier-curve [(c/v [(- parallel-face-end radius) (- (+ thickness radius)) 0])
                                                   (c/v [(- parallel-face-end radius) (- (+ thickness radius)) height])])
         f2-center-curve     (curves/bezier-curve [(c/v [0 (- (+ thickness radius)) 0])
                                                   (c/v [0 (- (+ thickness radius)) height])])
         ;; inner face
         face-1              (curves/clamped-b-spline-patch
                              {:control-curves [f1-r1-start-curve
                                                (with-meta f1-r1-middle-curve {:weight conics/WEIGHT_90})
                                                f1-r1-end-curve
                                                f1-center-curve
                                                (protocols/linear-transform f1-r1-end-curve (c/flip-matrix :x))
                                                (with-meta
                                                  (protocols/linear-transform f1-r1-middle-curve (c/flip-matrix :x))
                                                  {:weight conics/WEIGHT_90})
                                                (protocols/linear-transform f1-r1-start-curve (c/flip-matrix :x))]
                               :knot-vector    [1/3 1/3 2/3 2/3]
                               :order          2})
         ;; outer face
         face-2              (curves/clamped-b-spline-patch
                              {:control-curves [f2-start-curve
                                                f2-r1-start-curve
                                                (with-meta f2-r1-middle-curve {:weight conics/WEIGHT_90})
                                                f2-r1-end-curve
                                                f2-center-curve
                                                (protocols/linear-transform f2-r1-end-curve (c/flip-matrix :x))
                                                (with-meta
                                                  (protocols/linear-transform f2-r1-middle-curve (c/flip-matrix :x))
                                                  {:weight conics/WEIGHT_90})
                                                (protocols/linear-transform f2-r1-start-curve (c/flip-matrix :x))
                                                (protocols/linear-transform f2-start-curve (c/flip-matrix :x))]
                               :knot-vector    [1/5 2/5 2/5 3/5 3/5 4/5]
                               :order          2})]
     (gcode-layers/corrugated-panel-descriptor (protocols/linear-transform
                                                face-1
                                                (c/translate-matrix [(+ 150 wheel-center) 150 0]))
                                               (protocols/linear-transform
                                                face-2
                                                (c/translate-matrix [(+ 150 wheel-center) 150 0]))
                                               12
                                               (* layers-per-cm (/ height 10)) 100)
     #_(os/write
      (os/generate-polyhedron
       (protocols/triangle-mesh face-1 [2 100]))
      (os/generate-polyhedron
       (protocols/triangle-mesh face-2 [2 100]))))))

(comment
  (u/write-to-file
   "/Users/janherich/CAD/wheel_carriage.gcode"
   (gcode/generate-gcode (merge gcode/cr10-print-descriptor
                                (openbeam-plate-carriage 6 50)))))
