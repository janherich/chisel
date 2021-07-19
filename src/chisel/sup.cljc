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

(def ^:private layers-per-mm 5)

(def ^:private sup-length 3300)

(def low-volume-sup
  (let [right-deck-curve  (curves/clamped-uniform-b-spline
                           {:control-points [(c/v [300 0 0])
                                             (c/v [400 0 800])
                                             (c/v [400 0 2000])
                                             (c/v [5 0 sup-length])]
                            :order          3})
        right-chine-curve (curves/clamped-uniform-b-spline
                           {:control-points [(c/v [300 -60 0])
                                             (c/v [450 -100 800])
                                             (c/v [450 -100 2000])
                                             (c/v [5 -70 sup-length])]
                            :order          3})
        keel-curve        (curves/clamped-uniform-b-spline
                           {:control-points [(c/v [0 -60 0])
                                             (c/v [0 -100 800])
                                             (c/v [0 -100 2000])
                                             (c/v [0 -70 sup-length])]
                            :order          3})
        left-deck-curve   (protocols/linear-transform right-deck-curve (c/flip-matrix :x))
        left-chine-curve  (protocols/linear-transform right-chine-curve (c/flip-matrix :x))]
    (curves/clamped-uniform-b-spline-patch
     {:control-curves [(curves/unify-curve left-deck-curve sup-length)
                       (curves/unify-curve left-chine-curve sup-length)
                       (curves/unify-curve keel-curve sup-length)
                       (curves/unify-curve right-chine-curve sup-length)
                       (curves/unify-curve right-deck-curve sup-length)]
      :order           3})))

#_(def low-volume-sup-deck
  (let [right-deck-curve  (curves/clamped-uniform-b-spline
                           {:control-points [(c/v [290 0 0])
                                             (c/v [390 0 800])
                                             (c/v [390 0 2000])
                                             (c/v [0 0 sup-length])]
                            :order          3})
        right-chine-curve (curves/clamped-uniform-b-spline
                           {:control-points [(c/v [290 -50 0])
                                             (c/v [440 -90 800])
                                             (c/v [440 -90 2000])
                                             (c/v [0 -60 sup-length])]
                            :order          3})
        keel-curve        (curves/clamped-uniform-b-spline
                           {:control-points [(c/v [0 -50 0])
                                             (c/v [0 -90 800])
                                             (c/v [0 -90 2000])
                                             (c/v [0 -60 sup-length])]
                            :order          3})
        left-deck-curve   (protocols/linear-transform right-deck-curve (c/flip-matrix :x))
        left-chine-curve  (protocols/linear-transform right-chine-curve (c/flip-matrix :x))]
    (curves/clamped-uniform-b-spline-patch
     {:control-curves [(curves/unify-curve left-deck-curve sup-length)
                       (curves/unify-curve left-chine-curve sup-length)
                       (curves/unify-curve keel-curve sup-length)
                       (curves/unify-curve right-chine-curve sup-length)
                       (curves/unify-curve right-deck-curve sup-length)]
      :order           3})))


(def low-volume-sup-deck
  (let [right-deck-curve  (curves/clamped-uniform-b-spline
                           {:control-points [(c/v [300 0 0])
                                             (c/v [400 0 800])
                                             (c/v [400 0 2000])
                                             (c/v [5 0 sup-length])]
                            :order          3})
        left-deck-curve   (protocols/linear-transform right-deck-curve (c/flip-matrix :x))]
    (curves/bezier-patch
     [(curves/unify-curve left-deck-curve sup-length)
      (curves/unify-curve right-deck-curve sup-length)])))

(defn sup-render []
  (os/write
   (os/generate-polyhedron
    (protocols/triangle-mesh low-volume-sup [100 100]))
   (os/generate-polyhedron
    (protocols/triangle-mesh low-volume-sup-deck [100 100]))))

(defn sup-part-1 []
  (let [face-1 (protocols/linear-transform
                (curves/cut-patch low-volume-sup [2/3 1])
                (c/translate-matrix [0 0 (- (* 2 (/ sup-length 3)))]))
        face-2 (protocols/linear-transform
                (curves/cut-patch low-volume-sup-deck [2/3 1])
                (c/translate-matrix [0 0 (- (* 2 (/ sup-length 3)))]))]
    (merge (gcode-layers/double-corrugated-panel-descriptor
            face-1
            face-2
            15
            (* layers-per-mm (/ sup-length 3))
            200
            :modulate-curve (gcode-layers/sine-curve 1/40 15))
           {:skirt-polyline (gcode/circular-polyline 245 0)})
    #_(os/write
     (os/generate-polyhedron
      (protocols/triangle-mesh face-1 [100 100]))
     (os/generate-polyhedron
      (protocols/triangle-mesh face-2 [100 100])))))

(comment
  (u/write-to-file
   "/Users/janherich/CAD/SUP_part_1.gcode"
   (gcode/generate-gcode (merge gcode/bigprinter-print-descriptor (sup-part-1)))))

(defn sup-part-2 []
  (let [face-1 (protocols/linear-transform
                (curves/cut-patch low-volume-sup [1/3 2/3])
                (c/translate-matrix [0 0 (- (/ sup-length 3))]))
        face-2 (protocols/linear-transform
                (curves/cut-patch low-volume-sup-deck [1/3 2/3])
                (c/translate-matrix [0 0 (- (/ sup-length 3))]))]
    (merge (gcode-layers/double-corrugated-panel-descriptor
            face-1
            face-2
            15
            (* layers-per-mm (/ sup-length 3))
            200
            :modulate-curve (gcode-layers/sine-curve 1/40 15))
           {:skirt-polyline (gcode/circular-polyline 245 0)})
    #_(os/write
     (os/generate-polyhedron
      (protocols/triangle-mesh face-1 [100 100]))
     (os/generate-polyhedron
      (protocols/triangle-mesh face-2 [100 100])))))

(comment
  (u/write-to-file
   "/Users/janherich/CAD/SUP_part_2.gcode"
   (gcode/generate-gcode (merge gcode/bigprinter-print-descriptor (sup-part-2)))))

(defn sup-part-3 []
  (let [face-1 (curves/cut-patch low-volume-sup [0 1/3])
        face-2 (curves/cut-patch low-volume-sup-deck [0 1/3])]
    (merge (gcode-layers/double-corrugated-panel-descriptor
            face-1
            face-2
            15
            (* layers-per-mm (/ sup-length 3))
            200
            :modulate-curve (gcode-layers/sine-curve 1/40 15))
           {:skirt-polyline (gcode/circular-polyline 245 0)})
    #_(os/write
     (os/generate-polyhedron
      (protocols/triangle-mesh face-1 [100 100]))
     (os/generate-polyhedron
      (protocols/triangle-mesh face-2 [100 100])))))

(comment
  (u/write-to-file
   "/Users/janherich/CAD/SUP_part_3.gcode"
   (gcode/generate-gcode (merge gcode/bigprinter-print-descriptor (sup-part-3)))))
