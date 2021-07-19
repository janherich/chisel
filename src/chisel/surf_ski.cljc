(ns chisel.surf-ski
  "Low volume surf-ski"
  (:require [chisel.protocols :as protocols]
            [chisel.curves :as curves]
            [chisel.coordinates :as c]
            [chisel.utils :as u]
            [chisel.hull-utils :as h-u]
            [chisel.open-scad :as os]
            [chisel.gcode :as gcode]
            [chisel.gcode-layers :as gcode-layers]))

(def ^:private surfski-length 5500)

(def ^:private surfski-width 230)

(def ^:private surfski-depth 230)

(def ^:private surfski-parts 5)

(def ^:private surfski-mid-point (/ 24 55))

(def ^:private mid-point-absolute (* surfski-length surfski-mid-point))

(def ^:private part-length (/ surfski-length surfski-parts))

(comment
  right-top-curve   #_(curves/clamped-b-spline
                           {:control-points [(c/v [0 0 0])
                                             (c/v [0 0 0])
                                             (c/v [150 30 2300])
                                             (c/v [200 30 2400])
                                             (c/v [190 30 3100])
                                             (c/v [80 40 3400])
                                             (c/v [0 0 surfski-length])
                                             (c/v [0 0 surfski-length])]
                            :knot-vector    [1/4 1/4 2/4 3/4 3/4]
                            :order          2})
        (curves/clamped-uniform-b-spline
         {:control-points [(c/v [5 0 0])
                           (c/v [170 30 2200])
                           (c/v [170 30 2300])
                           (c/v [220 30 2400])
                           (c/v [210 30 3100])
                           (c/v [80 40 3400])
                           (c/v [80 40 3500])
                           (c/v [3 0 surfski-length])]
          :order          3}))

(def surfski
  (let [right-deck-curve  (curves/clamped-uniform-b-spline
                           {:control-points [(c/v [10 0 0])
                                             (c/v [(- surfski-width 20) 0 (- mid-point-absolute 900)])
                                             (c/v [surfski-width 0 mid-point-absolute])
                                             (c/v [surfski-width 0 (+ mid-point-absolute 50)])
                                             (c/v [10 0 surfski-length])]
                            :order          3})
        right-chine-curve (curves/clamped-uniform-b-spline
                           {:control-points [(c/v [10 -100 0])
                                             (c/v [surfski-width (- (- surfski-depth 120)) (- mid-point-absolute 900)])
                                             (c/v [surfski-width (- (- surfski-depth 100)) (+ mid-point-absolute 600)])
                                             (c/v [(- surfski-width 100) (- surfski-depth) (+ mid-point-absolute 900)])
                                             (c/v [10 -150 surfski-length])]
                            :order          3})
        keel-curve        (curves/clamped-uniform-b-spline
                           {:control-points [(c/v [0 -100 0])
                                             (c/v [0 (- (- surfski-depth 100)) (+ mid-point-absolute 800)])
                                             (c/v [0 (- surfski-depth) (+ mid-point-absolute 1300)])
                                             (c/v [0 -200 surfski-length])]
                            :order          3})
        left-deck-curve   (protocols/linear-transform right-deck-curve (c/flip-matrix :x))
        left-chine-curve  (protocols/linear-transform right-chine-curve (c/flip-matrix :x))]
    (curves/clamped-uniform-b-spline-patch
     {:control-curves [(curves/unify-curve left-deck-curve surfski-length)
                       (curves/unify-curve left-chine-curve surfski-length)
                       (curves/unify-curve keel-curve surfski-length)
                       (curves/unify-curve right-chine-curve surfski-length)
                       (curves/unify-curve right-deck-curve surfski-length)]
      ;;:knot-vector     [1/10 2/4 9/10]
      :order           3})))

(def surfski-inside
  (let [right-deck-curve  (curves/clamped-uniform-b-spline
                           {:control-points [(c/v [0 0 0])
                                             (c/v [(- surfski-width 30) 0 (- mid-point-absolute 900)])
                                             (c/v [(- surfski-width 10) 0 mid-point-absolute])
                                             (c/v [(- surfski-width 10) 0 (+ mid-point-absolute 50)])
                                             (c/v [0 0 surfski-length])]
                            :order          3})
        right-chine-curve (curves/clamped-uniform-b-spline
                           {:control-points [(c/v [0 -90 0])
                                             (c/v [(- surfski-width 10) (- (- surfski-depth 130)) (- mid-point-absolute 900)])
                                             (c/v [(- surfski-width 10) (- (- surfski-depth 110)) (+ mid-point-absolute 600)])
                                             (c/v [(- surfski-width 110) (- (- surfski-depth 10)) (+ mid-point-absolute 900)])
                                             (c/v [0 -140 surfski-length])]
                            :order          3})
        keel-curve        (curves/clamped-uniform-b-spline
                           {:control-points [(c/v [0 -90 0])
                                             (c/v [0 (- (- surfski-depth 110)) (+ mid-point-absolute 800)])
                                             (c/v [0 (- (- surfski-depth 10)) (+ mid-point-absolute 1300)])
                                             (c/v [0 -190 surfski-length])]
                            :order          3})
        left-deck-curve   (protocols/linear-transform right-deck-curve (c/flip-matrix :x))
        left-chine-curve  (protocols/linear-transform right-chine-curve (c/flip-matrix :x))]
    (curves/clamped-uniform-b-spline-patch
     {:control-curves [(curves/unify-curve left-deck-curve surfski-length)
                       (curves/unify-curve left-chine-curve surfski-length)
                       (curves/unify-curve keel-curve surfski-length)
                       (curves/unify-curve right-chine-curve surfski-length)
                       (curves/unify-curve right-deck-curve surfski-length)]
      ;;:knot-vector     [1/10 2/4 9/10]
      :order           3})))

(defn surfski-render []
  (os/write
   (os/generate-polyhedron
    (protocols/triangle-mesh (curves/cut-patch surfski [0 1 #_(/ 11 20)]) [100 100]))
   (os/generate-polyhedron
    (protocols/triangle-mesh (curves/cut-patch surfski-inside [0 1 #_(/ 11 20)]) [100 100]))))

(comment
  (u/write-to-file
   "/Users/janherich/CAD/surfski_1.gcode"
   (gcode/generate-gcode (merge gcode/bigprinter-print-descriptor
                                ;;gcode/pa12-gf-print-descriptor
                                (gcode-layers/corrugated-panel
                                 (curves/patch-part surfski surfski-parts 0
                                                    :y-offset 50
                                                    :mid-point surfski-mid-point)
                                 (curves/patch-part surfski-inside surfski-parts 0
                                                    :y-offset 50
                                                    :mid-point surfski-mid-point)
                                 :corrugate-fn    gcode-layers/sine-corrugations
                                 :overlap         0
                                 :core-line-width 5/10
                                 :skin-line-width 4/10)))))

(comment
  (u/write-to-file
   "/Users/janherich/CAD/surfski_2.gcode"
   (gcode/generate-gcode (merge gcode/bigprinter-print-descriptor
                                ;;gcode/pa12-gf-print-descriptor
                                (gcode-layers/corrugated-panel
                                 (curves/patch-part surfski surfski-parts 1
                                                    :y-offset 30
                                                    :mid-point surfski-mid-point)
                                 (curves/patch-part surfski-inside surfski-parts 1
                                                    :y-offset 30
                                                    :mid-point surfski-mid-point)
                                 :corrugate-fn    gcode-layers/sine-corrugations
                                 :overlap         0
                                 :core-line-width 5/10
                                 :skin-line-width 4/10)))))

(comment
  (u/write-to-file
   "/Users/janherich/CAD/surfski_3.gcode"
   (gcode/generate-gcode (merge gcode/bigprinter-print-descriptor
                                ;;gcode/pa12-gf-print-descriptor
                                (gcode-layers/corrugated-panel
                                 (curves/patch-part surfski surfski-parts 2
                                                    :y-offset 30
                                                    :mid-point surfski-mid-point)
                                 (curves/patch-part surfski-inside surfski-parts 2
                                                    :y-offset 30
                                                    :mid-point surfski-mid-point)
                                 :corrugate-fn    gcode-layers/sine-corrugations
                                 :overlap         0
                                 :core-line-width 5/10
                                 :skin-line-width 4/10)))))

(comment
  (u/write-to-file
   "/Users/janherich/CAD/surfski_4.gcode"
   (gcode/generate-gcode (merge gcode/bigprinter-print-descriptor
                                ;;gcode/pa12-gf-print-descriptor
                                (gcode-layers/corrugated-panel
                                 (curves/patch-part surfski surfski-parts 3
                                                    :y-offset 10
                                                    :mid-point surfski-mid-point)
                                 (curves/patch-part surfski-inside surfski-parts 3
                                                    :y-offset 10
                                                    :mid-point surfski-mid-point)
                                 :corrugate-fn    gcode-layers/sine-corrugations
                                 :overlap         0
                                 :core-line-width 5/10
                                 :skin-line-width 4/10)))))

(comment
  (u/write-to-file
   "/Users/janherich/CAD/surfski_5.gcode"
   (gcode/generate-gcode (merge gcode/bigprinter-print-descriptor
                                ;;gcode/pa12-gf-print-descriptor
                                (gcode-layers/corrugated-panel
                                 (curves/patch-part surfski surfski-parts 4
                                                    :y-offset 80
                                                    :mid-point surfski-mid-point)
                                 (curves/patch-part surfski-inside surfski-parts 4
                                                    :y-offset 80
                                                    :mid-point surfski-mid-point)
                                 :corrugate-fn    gcode-layers/sine-corrugations
                                 :overlap         0
                                 :core-line-width 45/100
                                 :skin-line-width 35/100)))))
