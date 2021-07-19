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

(def ^:private paddle-length 300)

(def ^:private paddle-width  100)

(def ^:private shaft-r 10)

(def ^:private paddle-parts 2)

(def ^:private paddle-mid-point 1/2)

(def ^:private mid-point-absolute (* paddle-length paddle-mid-point))

(def ^:private part-length (/ paddle-length paddle-parts))

(def sup-blade-top
  (let [half-width              (/ paddle-width 2)
        right-border-curve      (curves/clamped-uniform-b-spline
                                 {:control-points [(c/v [0 half-width 0])
                                                   (c/v [0 half-width (* paddle-length 5/11)])
                                                   (c/v [0 (* half-width 7/8) (* paddle-length 25/44)])
                                                   (c/v [0 shaft-r (* paddle-length 10/11)])
                                                   (c/v [0 shaft-r paddle-length])]
                                  :order 3})
        right-border-lift-curve (curves/clamped-uniform-b-spline
                                 {:control-points [(c/v [0 (* half-width 7/8) 0])
                                                   (c/v [4 (* half-width 7/8) 0])
                                                   (c/v [5 (* half-width 7/8) (* paddle-length 5/11)])
                                                   (c/v [7 (* half-width 7/8) (* paddle-length 25/44)])
                                                   (c/v [shaft-r shaft-r (* paddle-length 10/11)])
                                                   (c/v [shaft-r shaft-r paddle-length])]
                                  :order 3})
        spine-curve             (curves/clamped-uniform-b-spline
                                 {:control-points [(c/v [0 0 0])
                                                   (c/v [4 0 0])
                                                   (c/v [(* shaft-r 2/3) 0 (* paddle-length 5/11)])
                                                   (c/v [(* shaft-r 2/3) 0 (* paddle-length 25/44)])
                                                   (c/v [shaft-r 0 (* paddle-length 10/11)])
                                                   (c/v [shaft-r 0 paddle-length])]
                                  :order          3})
        left-border-curve       (protocols/linear-transform right-border-curve (c/flip-matrix :y))
        left-border-lift-curve  (protocols/linear-transform right-border-lift-curve (c/flip-matrix :y))]
    (curves/clamped-b-spline-patch
     {:control-curves [(curves/unify-curve left-border-curve paddle-length)
                       (with-meta
                         (curves/unify-curve left-border-lift-curve paddle-length)
                         {:weight conics/WEIGHT_90})
                       (curves/unify-curve spine-curve paddle-length)
                       (with-meta
                         (curves/unify-curve right-border-lift-curve paddle-length)
                         {:weight conics/WEIGHT_90})
                       (curves/unify-curve right-border-curve paddle-length)]
      :knot-vector    [1/2 1/2]
      :order          2})))

(def sup-blade-bottom
  (protocols/linear-transform sup-blade-top (c/flip-matrix :x)))

(defn paddle-render []
  (os/write
   (os/generate-polyhedron
    (protocols/triangle-mesh sup-blade-top [100 100]))
   (os/generate-polyhedron
    (protocols/triangle-mesh sup-blade-bottom [100 100]))))

(comment
  (u/write-to-file
   "/Users/janherich/CAD/paddle_top.gcode"
   (gcode/generate-gcode (merge gcode/deltav1-print-descriptor
                                (gcode-layers/corrugated-panel
                                 (curves/patch-part sup-blade-top paddle-parts 0
                                                    :mid-point paddle-mid-point)
                                 (curves/patch-part sup-blade-bottom paddle-parts 0
                                                    :mid-point paddle-mid-point)
                                 :corrugate-fn     gcode-layers/sine-corrugations
                                 :corrugation-size 10
                                 :core-line-width  45/100
                                 :skin-line-width  35/100)))))

(comment
  (u/write-to-file
   "/Users/janherich/CAD/paddle_bottom.gcode"
   (gcode/generate-gcode (merge gcode/deltav1-print-descriptor
                                (gcode-layers/corrugated-panel
                                 (curves/patch-part sup-blade-top paddle-parts 1
                                                    :mid-point paddle-mid-point)
                                 (curves/patch-part sup-blade-bottom paddle-parts 1
                                                    :mid-point paddle-mid-point)
                                 :corrugate-fn     gcode-layers/sine-corrugations
                                 :corrugation-size 10
                                 :core-line-width  45/100
                                 :skin-line-width  35/100)))))

