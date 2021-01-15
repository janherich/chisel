(ns chisel.sup
  "Low volume SUP"
  (:require [chisel.protocols :as protocols]
            [chisel.curves :as curves]
            [chisel.coordinates :as c]
            [chisel.utils :as u]
            [chisel.hull-utils :as h-u]
            [chisel.open-scad :as os]
            [chisel.gcode :as gcode]
            [chisel.gcode-layers :as gcode-layers]))

(def ^:private length-cm 320)

(def low-volume-sup
  (let [overall-length    (* 10 length-cm)
        right-deck-curve  (curves/clamped-uniform-b-spline
                           {:control-points [(c/v [300 0 0])
                                             (c/v [400 0 800])
                                             (c/v [400 0 2000])
                                             (c/v [5 0 overall-length])]
                            :order          3})
        right-chine-curve (curves/clamped-uniform-b-spline
                           {:control-points [(c/v [300 -60 0])
                                             (c/v [450 -100 800])
                                             (c/v [450 -100 2000])
                                             (c/v [5 -70 overall-length])]
                            :order          3})
        keel-curve        (curves/clamped-uniform-b-spline
                           {:control-points [(c/v [0 -60 0])
                                             (c/v [0 -100 800])
                                             (c/v [0 -100 2000])
                                             (c/v [0 -70 overall-length])]
                            :order          3})
        left-deck-curve   (protocols/linear-transform right-deck-curve (c/flip-matrix :x))
        left-chine-curve  (protocols/linear-transform right-chine-curve (c/flip-matrix :x))]
    (curves/clamped-uniform-b-spline-patch
     {:control-curves [(curves/unify-curve left-deck-curve overall-length)
                       (curves/unify-curve left-chine-curve overall-length)
                       (curves/unify-curve keel-curve overall-length)
                       (curves/unify-curve right-chine-curve overall-length)
                       (curves/unify-curve right-deck-curve overall-length)]
      :order           3})))

(def low-volume-sup-deck
  (let [overall-length    (* 10 length-cm)
        right-deck-curve  (curves/clamped-uniform-b-spline
                           {:control-points [(c/v [300 0 0])
                                             (c/v [400 0 800])
                                             (c/v [400 0 2000])
                                             (c/v [5 0 overall-length])]
                            :order          3})
        left-deck-curve   (protocols/linear-transform right-deck-curve (c/flip-matrix :x))]
    (curves/bezier-patch
     [(curves/unify-curve left-deck-curve overall-length)
      (curves/unify-curve right-deck-curve overall-length)])))


(def ^:private layers-per-cm 50)

(defn sup-top []
  (let [face-1 (protocols/linear-transform
                (curves/cut-patch low-volume-sup [1/2 1])
                (c/translate-matrix [0 0 (- (/ (* 10 length-cm) 2))]))
        face-2 (protocols/linear-transform
                (curves/cut-patch low-volume-sup-deck [1/2 1])
                (c/translate-matrix [0 0 (- (/ (* 10 length-cm) 2))]))]
    (merge (gcode-layers/corrugated-panel-descriptor face-1 face-2 20
                                                     (* layers-per-cm (/ length-cm 2))
                                                     200)
           {:skirt-polyline (gcode/circular-polyline 350 0)})
    #_(os/write
     (os/generate-polyhedron
      (protocols/triangle-mesh face-1 [100 100]))
     (os/generate-polyhedron
      (protocols/triangle-mesh face-2 [100 100])))))

(defn sup-bottom []
  (let [face-1 (curves/cut-patch low-volume-sup [0 1/2])
        face-2 (curves/cut-patch low-volume-sup-deck [0 1/2])]
    (merge (gcode-layers/corrugated-panel-descriptor face-1 face-2 20
                                                     (* layers-per-cm (/ length-cm 2))
                                                     200)
           {:skirt-polyline (gcode/circular-polyline 350 0)})
    #_(os/write
     (os/generate-polyhedron
      (protocols/triangle-mesh face-1 [100 100]))
     (os/generate-polyhedron
      (protocols/triangle-mesh face-2 [100 100])))))
