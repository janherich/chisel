(ns chisel.skateboard
  "2 wheeled skateboard"
  (:require [chisel.protocols :as protocols]
            [chisel.curves :as curves]
            [chisel.coordinates :as c]
            [chisel.utils :as u]
            [chisel.open-scad :as os]
            [chisel.gcode :as gcode]
            [chisel.gcode-layers :as gcode-layers]))

(def ^:private layers-per-mm 5)

(def ^:private skateboard-length 700)

(def ^:private skateboard-width 200)

(def ^:private skateboard-depth 40)

(def ^:private wheel-diameter 100)

(def ^:private half-width (/ skateboard-width 2))

(def ^:private right-deck-curve
  (curves/clamped-uniform-b-spline
   {:control-points [(c/v [30 0 0])
                     (c/v [half-width 0 0])
                     (c/v [half-width 0 (* 2 half-width)])
                     (c/v [half-width 0 (- skateboard-length (* 2 half-width) wheel-diameter)])
                     (c/v [half-width 0 (- skateboard-length wheel-diameter)])
                     (c/v [0 0 (- skateboard-length wheel-diameter)])
                     (c/v [-10 0 (- skateboard-length wheel-diameter)])
                     (c/v [-10 0 (- skateboard-length (- wheel-diameter 10))])
                     (c/v [-10 0 skateboard-length])]
    :order          3}))

(def ^:private left-deck-curve
  (curves/clamped-uniform-b-spline
   {:control-points [(c/v [10 0 0])
                     (c/v [10 0 (- wheel-diameter 10)])
                     (c/v [10 0 wheel-diameter])
                     (c/v [0 0 wheel-diameter])
                     (c/v [(- half-width) 0 wheel-diameter])
                     (c/v [(- half-width) 0 (+ wheel-diameter (* 2 half-width))])
                     (c/v [(- half-width) 0 (- skateboard-length (* 2 half-width))])
                     (c/v [(- half-width) 0 skateboard-length])
                     (c/v [-30 0 skateboard-length])]
    :order          3}))

(def skateboard
  (let [right-chine-curve (protocols/linear-transform
                           right-deck-curve (c/translate-matrix [0 skateboard-depth 0]))
        bottom-curve      (curves/clamped-uniform-b-spline
                           {:control-points [(c/v [15 skateboard-depth 0])
                                             (c/v [15 skateboard-depth (- wheel-diameter 10)])
                                             (c/v [15 skateboard-depth wheel-diameter])
                                             (c/v [-15 skateboard-depth (- skateboard-length
                                                                         wheel-diameter)])
                                             (c/v [-15 skateboard-depth (- skateboard-length
                                                                           (- wheel-diameter 10))])
                                             (c/v [-15 skateboard-depth skateboard-length])]
                            :order          3})
        left-chine-curve  (protocols/linear-transform
                           left-deck-curve (c/translate-matrix [0 skateboard-depth 0]))]
    (curves/clamped-uniform-b-spline-patch
     {:control-curves [(curves/unify-curve left-deck-curve skateboard-length)
                       (curves/unify-curve left-chine-curve skateboard-length)
                       (curves/unify-curve bottom-curve skateboard-length)
                       (curves/unify-curve right-chine-curve skateboard-length)
                       (curves/unify-curve right-deck-curve skateboard-length)]
      :order           3})))

(def skateboard-deck
  (curves/bezier-patch [(curves/unify-curve left-deck-curve skateboard-length)
                        (curves/unify-curve right-deck-curve skateboard-length)]))

(defn skateboard-render []
  (os/write
   (os/generate-polyhedron
    (protocols/triangle-mesh skateboard [100 100]))
   (os/generate-polyhedron
    (protocols/triangle-mesh skateboard-deck [100 100]))))

(comment
  (u/write-to-file
   "/Users/janherich/CAD/skateboard_half.gcode"
   (gcode/generate-gcode (merge gcode/deltav1-print-descriptor
                                (gcode-layers/corrugated-panel
                                 (curves/patch-part skateboard 2 0)
                                 (curves/patch-part skateboard-deck 2 0)
                                 :core-line-width 5/10
                                 :skin-line-width 4/10)))))

