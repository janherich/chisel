(ns chisel.kayak
  "Low volume kayak"
  (:require [chisel.protocols :as protocols]
            [chisel.curves :as curves]
            [chisel.coordinates :as c]
            [chisel.utils :as u]
            [chisel.hull-utils :as h-u]
            [chisel.open-scad :as os]
            [chisel.gcode :as gcode]
            [chisel.gcode-layers :as gcode-layers]))

(def ^:private length-cm 520)

(def low-volume-kayak
  (let [overall-length    (* 10 length-cm)
        right-top-curve   (curves/clamped-b-spline
                           {:control-points [(c/v [0 0 0])
                                             (c/v [0 0 0])
                                             (c/v [0 0 500])
                                             (c/v [80 0 600])
                                             (c/v [0 0 700])
                                             (c/v [0 0 700])
                                             (c/v [0 0 2300])
                                             (c/v [200 0 2400])
                                             (c/v [190 0 3100])
                                             (c/v [0 40 3400])
                                             (c/v [0 0 overall-length])
                                             (c/v [0 0 overall-length])]
                            :knot-vector    [1/6 1/6 2/6 2/6 3/6 3/6 4/6 5/6 5/6]
                            :order          2})
        right-deck-curve  (curves/clamped-uniform-b-spline
                           {:control-points [(c/v [10 0 0])
                                             (c/v [200 0 1500])
                                             (c/v [220 0 2400])
                                             (c/v [220 0 2450])
                                             (c/v [5 0 overall-length])]
                            :order          3})
        right-chine-curve (curves/clamped-uniform-b-spline
                           {:control-points [(c/v [10 -100 0])
                                             (c/v [250 -120 1500])
                                             (c/v [250 -140 3000])
                                             (c/v [170 -230 3300])
                                             (c/v [5 -150 overall-length])]
                            :order          3})
        keel-curve        (curves/clamped-uniform-b-spline
                           {:control-points [(c/v [0 -120 0])
                                             (c/v [0 -140 3200])
                                             (c/v [0 -270 3700])
                                             (c/v [0 -200 overall-length])]
                            :order          3})
        left-top-curve    (protocols/linear-transform right-top-curve (c/flip-matrix :x))
        left-deck-curve   (protocols/linear-transform right-deck-curve (c/flip-matrix :x))
        left-chine-curve  (protocols/linear-transform right-chine-curve (c/flip-matrix :x))]
    (curves/clamped-b-spline-patch
     {:control-curves [(curves/unify-curve left-top-curve overall-length)
                       (curves/unify-curve left-deck-curve overall-length)
                       (curves/unify-curve left-chine-curve overall-length)
                       (curves/unify-curve keel-curve overall-length)
                       (curves/unify-curve right-chine-curve overall-length)
                       (curves/unify-curve right-deck-curve overall-length)
                       (curves/unify-curve right-top-curve overall-length)]
      :knot-vector     [1/10 2/4 9/10]
      :order           3})))

(def low-volume-kayak-inside
  (let [overall-length    5200
        right-top-curve   (curves/clamped-b-spline
                           {:control-points [(c/v [0 -7 0])
                                             (c/v [0 -7 0])
                                             (c/v [0 -7 500])
                                             (c/v [80 -7 600])
                                             (c/v [0 -7 700])
                                             (c/v [0 -7 700])
                                             (c/v [0 -7 2300])
                                             (c/v [190 -7 2400])
                                             (c/v [180 -7 3100])
                                             (c/v [0 33 3400])
                                             (c/v [0 -7 overall-length])
                                             (c/v [0 -7 overall-length])]
                            :knot-vector    [1/6 1/6 2/6 2/6 3/6 3/6 4/6 5/6 5/6]
                            :order          2})
        right-deck-curve  (curves/clamped-uniform-b-spline
                           {:control-points [(c/v [0 -7 0])
                                             (c/v [190 -7 1500])
                                             (c/v [210 -7 2400])
                                             (c/v [210 -7 2450])
                                             (c/v [0 -7 overall-length])]
                            :order          3})
        right-chine-curve (curves/clamped-uniform-b-spline
                           {:control-points [(c/v [0 -93 0])
                                             (c/v [240 -113 1500])
                                             (c/v [240 -133 3000])
                                             (c/v [160 -223 3300])
                                             (c/v [0 -143 overall-length])]
                            :order          3})
        keel-curve        (curves/clamped-uniform-b-spline
                           {:control-points [(c/v [0 -113 0])
                                             (c/v [0 -133 3200])
                                             (c/v [0 -263 3700])
                                             (c/v [0 -193 overall-length])]
                            :order          3})
        left-top-curve    (protocols/linear-transform right-top-curve (c/flip-matrix :x))
        left-deck-curve   (protocols/linear-transform right-deck-curve (c/flip-matrix :x))
        left-chine-curve  (protocols/linear-transform right-chine-curve (c/flip-matrix :x))]
    (curves/clamped-b-spline-patch
     {:control-curves [(curves/unify-curve left-top-curve overall-length)
                       (curves/unify-curve left-deck-curve overall-length)
                       (curves/unify-curve left-chine-curve overall-length)
                       (curves/unify-curve keel-curve overall-length)
                       (curves/unify-curve right-chine-curve overall-length)
                       (curves/unify-curve right-deck-curve overall-length)
                       (curves/unify-curve right-top-curve overall-length)]
      :knot-vector     [1/10 2/4 9/10]
      :order           3})))

(def ^:private layers-per-cm 50)

(defn part-1 [right?]
  (let [start-cm   487
        end-cm     length-cm
        interval-1 [(/ start-cm length-cm) (/ end-cm length-cm)]
        interval-2 (if right? [0 1/2] [1/2 1])
        outer-skin (protocols/linear-transform
                    (curves/cut-patch low-volume-kayak interval-1 #_interval-2)
                    (c/translate-matrix [0 90 (- (* 10 start-cm))]))
        inner-skin (protocols/linear-transform
                    (curves/cut-patch low-volume-kayak-inside interval-1 #_interval-2)
                    (c/translate-matrix [0 90 (- (* 10 start-cm))]))]
    (merge (gcode-layers/corrugated-panel-descriptor outer-skin inner-skin 60
                                                     (* layers-per-cm (- end-cm start-cm))
                                                     300)
           {:skirt-polyline [[-40 110] [-40 -110] [40 -110] [40 110] [-40 110]]})
    #_(os/write
     (os/generate-polyhedron
      (protocols/triangle-mesh outer-skin [100 100]))
     (os/generate-polyhedron
      (protocols/triangle-mesh inner-skin [100 100])))))

(defn part-2 [right?]
  (let [start-cm   454
        end-cm     487
        interval-1 [(/ start-cm length-cm) (/ end-cm length-cm)]
        interval-2 (if right? [0 1/2] [1/2 1])
        outer-skin (protocols/linear-transform
                    (curves/cut-patch low-volume-kayak interval-1 #_interval-2)
                    (c/translate-matrix [0 93 (- (* 10 start-cm))]))
        inner-skin (protocols/linear-transform
                    (curves/cut-patch low-volume-kayak-inside interval-1 #_interval-2)
                    (c/translate-matrix [0 93 (- (* 10 start-cm))]))]
    (merge (gcode-layers/corrugated-panel-descriptor outer-skin inner-skin 60
                                                     (* layers-per-cm (- end-cm start-cm))
                                                     300)
           {:skirt-polyline [[-60 110] [-60 -110] [60 -110] [60 110] [-60 110]]})
    #_(os/write
     (os/generate-polyhedron
      (protocols/triangle-mesh outer-skin [100 100]))
     (os/generate-polyhedron
      (protocols/triangle-mesh inner-skin [100 100])))))

(defn part-3 [right?]
  (let [start-cm   421
        end-cm     454
        interval-1 [(/ start-cm length-cm) (/ end-cm length-cm)]
        interval-2 (if right? [0 1/2] [1/2 1])
        outer-skin (protocols/linear-transform
                    (curves/cut-patch low-volume-kayak interval-1 #_interval-2)
                    (c/translate-matrix [0 93 (- (* 10 start-cm))]))
        inner-skin (protocols/linear-transform
                    (curves/cut-patch low-volume-kayak-inside interval-1 #_interval-2)
                    (c/translate-matrix [0 93 (- (* 10 start-cm))]))]
    (merge (gcode-layers/corrugated-panel-descriptor outer-skin inner-skin 60
                                                     (* layers-per-cm (- end-cm start-cm))
                                                     400)
           {:skirt-polyline [[0 125] [-90 80] [-90 -80] [0 -125] [90 -80] [90 80] [0 125]]})
    #_(os/write
     (os/generate-polyhedron
      (protocols/triangle-mesh outer-skin [100 100]))
     (os/generate-polyhedron
      (protocols/triangle-mesh inner-skin [100 100])))))

(defn part-4 [right?]
  (let [start-cm   388
        end-cm     421
        interval-1 [(/ start-cm length-cm) (/ end-cm length-cm)]
        interval-2 (if right? [0 1/2] [1/2 1])
        outer-skin (protocols/linear-transform
                    (curves/cut-patch low-volume-kayak interval-1 #_interval-2)
                    (c/translate-matrix [0 93 (- (* 10 start-cm))]))
        inner-skin (protocols/linear-transform
                    (curves/cut-patch low-volume-kayak-inside interval-1 #_interval-2)
                    (c/translate-matrix [0 93 (- (* 10 start-cm))]))]
    (merge (gcode-layers/corrugated-panel-descriptor outer-skin inner-skin 60
                                                     (* layers-per-cm (- end-cm start-cm))
                                                     400)
           {:skirt-polyline (gcode/circular-polyline 132)})
    #_(os/write
     (os/generate-polyhedron
      (protocols/triangle-mesh outer-skin [100 100]))
     (os/generate-polyhedron
      (protocols/triangle-mesh inner-skin [100 100])))))

(defn part-5 [right?]
  (let [start-cm   355
        end-cm     388
        interval-1 [(/ start-cm length-cm) (/ end-cm length-cm)]
        interval-2 (if right? [0 1/2] [1/2 1])
        side-shift (if right? 25 -25)
        outer-skin (protocols/linear-transform
                    (curves/cut-patch low-volume-kayak interval-1 interval-2)
                    (c/translate-matrix [side-shift 88 (- (* 10 start-cm))]))
        inner-skin (protocols/linear-transform
                    (curves/cut-patch low-volume-kayak-inside interval-1 interval-2)
                    (c/translate-matrix [side-shift 88 (- (* 10 start-cm))]))]
    #_(merge (gcode-layers/corrugated-panel-descriptor outer-skin inner-skin 30
                                                     (* layers-per-cm (- end-cm start-cm))
                                                     300)
           {:skirt-polyline (gcode/circular-polyline 132 (if right? Math/PI 0))})
    (os/write
     (os/generate-polyhedron
      (protocols/triangle-mesh outer-skin [100 100]))
     "circle(132, $fn=100);"
     #_(os/generate-polyhedron
        (protocols/triangle-mesh inner-skin [100 100])))))

(defn part-6 [right?]
  (let [start-cm   323
        end-cm     355
        interval-1 [(/ start-cm length-cm) (/ end-cm length-cm)]
        interval-2 (if right? [0 1/2] [1/2 1])
        side-shift (if right? 42 -42)
        outer-skin (protocols/linear-transform
                    (curves/cut-patch low-volume-kayak interval-1 interval-2)
                    (c/translate-matrix [side-shift 80 (- (* 10 start-cm))]))
        inner-skin (protocols/linear-transform
                    (curves/cut-patch low-volume-kayak-inside interval-1 interval-2)
                    (c/translate-matrix [side-shift 80 (- (* 10 start-cm))]))]
    (merge (gcode-layers/corrugated-panel-descriptor outer-skin inner-skin 30
                                                     (* layers-per-cm (- end-cm start-cm))
                                                     300)
           {:skirt-polyline (gcode/circular-polyline 132 (if right? Math/PI 0))})
    #_(os/write
     (os/generate-polyhedron
      (protocols/triangle-mesh outer-skin [100 100]))
     "circle(132, $fn=100);"
     #_(os/generate-polyhedron
        (protocols/triangle-mesh inner-skin [100 100])))))

(defn part-7 [right?]
  (let [start-cm   295
        end-cm     323
        interval-1 [(/ start-cm length-cm) (/ end-cm length-cm)]
        interval-2 (if right? [0 1/2] [1/2 1])
        side-shift (if right? 90 -90)
        outer-skin (protocols/linear-transform
                    (curves/cut-patch low-volume-kayak interval-1 interval-2)
                    (c/translate-matrix [side-shift 96.5 (- (* 10 start-cm))]))
        inner-skin (protocols/linear-transform
                    (curves/cut-patch low-volume-kayak-inside interval-1 interval-2)
                    (c/translate-matrix [side-shift 96.5 (- (* 10 start-cm))]))]
    (merge (gcode-layers/corrugated-panel-descriptor outer-skin inner-skin 30
                                                     (* layers-per-cm (- end-cm start-cm))
                                                     300)
           {:skirt-polyline (gcode/circular-polyline 132 (if right? Math/PI 0))})
    #_(os/write
     (os/generate-polyhedron
      (protocols/triangle-mesh outer-skin [100 100]))
     "circle(132.2, $fn=100);"
     #_(os/generate-polyhedron
        (protocols/triangle-mesh inner-skin [100 100])))))

(defn part-8 [right?]
  (let [start-cm   263
        end-cm     295
        interval-1 [(/ start-cm length-cm) (/ end-cm length-cm)]
        interval-2 (if right? [0 1/2] [1/2 1])
        side-shift (if right? 98.5 -98.5)
        outer-skin (protocols/linear-transform
                    (curves/cut-patch low-volume-kayak interval-1 interval-2)
                    (c/translate-matrix [side-shift 92 (- (* 10 start-cm))]))
        inner-skin (protocols/linear-transform
                    (curves/cut-patch low-volume-kayak-inside interval-1 interval-2)
                    (c/translate-matrix [side-shift 92 (- (* 10 start-cm))]))]
    (merge (gcode-layers/corrugated-panel-descriptor outer-skin inner-skin 30
                                                     (* layers-per-cm (- end-cm start-cm))
                                                     300)
           {:skirt-polyline (gcode/circular-polyline 132.2 (if right? Math/PI 0))})
    #_(os/write
     (os/generate-polyhedron
      (protocols/triangle-mesh outer-skin [100 100]))
     "circle(132.2, $fn=100);"
     #_(os/generate-polyhedron
        (protocols/triangle-mesh inner-skin [100 100])))))

(defn part-9 [right?]
  (let [start-cm   230
        end-cm     263
        interval-1 [(/ start-cm length-cm) (/ end-cm length-cm)]
        interval-2 (if right? [0 1/2] [1/2 1])
        side-shift (if right? 100 -100)
        outer-skin (protocols/linear-transform
                    (curves/cut-patch low-volume-kayak interval-1 interval-2)
                    (c/translate-matrix [side-shift 85 (- (* 10 start-cm))]))
        inner-skin (protocols/linear-transform
                    (curves/cut-patch low-volume-kayak-inside interval-1 interval-2)
                    (c/translate-matrix [side-shift 85 (- (* 10 start-cm))]))]
    (merge (gcode-layers/corrugated-panel-descriptor outer-skin inner-skin 30
                                                     (* layers-per-cm (- end-cm start-cm))
                                                     300)
           {:skirt-polyline (gcode/circular-polyline 130 (if right? Math/PI 0))})
    #_(os/write
     (os/generate-polyhedron
      (protocols/triangle-mesh outer-skin [100 100]))
     "circle(132.2, $fn=100);"
     #_(os/generate-polyhedron
        (protocols/triangle-mesh inner-skin [100 100])))))

(defn part-10 [right?]
  (let [start-cm   197
        end-cm     230
        interval-1 [(/ start-cm length-cm) (/ end-cm length-cm)]
        interval-2 (if right? [0 1/2] [1/2 1])
        side-shift (if right? 100 -100)
        outer-skin (protocols/linear-transform
                    (curves/cut-patch low-volume-kayak interval-1 interval-2)
                    (c/translate-matrix [side-shift 75 (- (* 10 start-cm))]))
        inner-skin (protocols/linear-transform
                    (curves/cut-patch low-volume-kayak-inside interval-1 interval-2)
                    (c/translate-matrix [side-shift 75 (- (* 10 start-cm))]))]
    (merge (gcode-layers/corrugated-panel-descriptor outer-skin inner-skin 30
                                                     (* layers-per-cm (- end-cm start-cm))
                                                     300)
           {:skirt-polyline (gcode/circular-polyline 130 (if right? Math/PI 0))})
    #_(os/write
     (os/generate-polyhedron
      (protocols/triangle-mesh outer-skin [100 100]))
     "circle(132.2, $fn=100);"
     #_(os/generate-polyhedron
        (protocols/triangle-mesh inner-skin [100 100])))))

(defn part-11 [right?]
  (let [start-cm   164
        end-cm     197
        interval-1 [(/ start-cm length-cm) (/ end-cm length-cm)]
        interval-2 (if right? [0 1/2] [1/2 1])
        side-shift (if right? 100 -100)
        outer-skin (protocols/linear-transform
                    (curves/cut-patch low-volume-kayak interval-1 interval-2)
                    (c/translate-matrix [side-shift 75 (- (* 10 start-cm))]))
        inner-skin (protocols/linear-transform
                    (curves/cut-patch low-volume-kayak-inside interval-1 interval-2)
                    (c/translate-matrix [side-shift 75 (- (* 10 start-cm))]))]
    (merge (gcode-layers/corrugated-panel-descriptor outer-skin inner-skin 30
                                                     (* layers-per-cm (- end-cm start-cm))
                                                     300)
           {:skirt-polyline (gcode/circular-polyline 130 (if right? Math/PI 0))})
    #_(os/write
     (os/generate-polyhedron
      (protocols/triangle-mesh outer-skin [100 100]))
     "circle(132.2, $fn=100);"
     #_(os/generate-polyhedron
        (protocols/triangle-mesh inner-skin [100 100])))))

(defn part-12 [right?]
  (let [start-cm   121
        end-cm     164
        interval-1 [(/ start-cm length-cm) (/ end-cm length-cm)]
        interval-2 (if right? [0 1/2] [1/2 1])
        side-shift (if right? 80 -80)
        outer-skin (protocols/linear-transform
                    (curves/cut-patch low-volume-kayak interval-1 interval-2)
                    (c/translate-matrix [side-shift 75 (- (* 10 start-cm))]))
        inner-skin (protocols/linear-transform
                    (curves/cut-patch low-volume-kayak-inside interval-1 interval-2)
                    (c/translate-matrix [side-shift 75 (- (* 10 start-cm))]))]
    (merge (gcode-layers/corrugated-panel-descriptor outer-skin inner-skin 30
                                                     (* layers-per-cm (- end-cm start-cm))
                                                     300)
           {:skirt-polyline (gcode/circular-polyline 130 (if right? Math/PI 0))})
    #_(os/write
     (os/generate-polyhedron
      (protocols/triangle-mesh outer-skin [100 100]))
     "circle(132.2, $fn=100);"
     #_(os/generate-polyhedron
        (protocols/triangle-mesh inner-skin [100 100])))))

(defn part-13 [right?]
  (let [start-cm   88
        end-cm     121
        interval-1 [(/ start-cm length-cm) (/ end-cm length-cm)]
        interval-2 (if right? [0 1/2] [1/2 1])
        side-shift (if right? 50 -50)
        outer-skin (protocols/linear-transform
                    (curves/cut-patch low-volume-kayak interval-1 interval-2)
                    (c/translate-matrix [side-shift 60 (- (* 10 start-cm))]))
        inner-skin (protocols/linear-transform
                    (curves/cut-patch low-volume-kayak-inside interval-1 interval-2)
                    (c/translate-matrix [side-shift 60 (- (* 10 start-cm))]))]
    (merge (gcode-layers/corrugated-panel-descriptor outer-skin inner-skin 30
                                                     (* layers-per-cm (- end-cm start-cm))
                                                     300)
           {:skirt-polyline (gcode/circular-polyline 130 (if right? Math/PI 0))})
    #_(os/write
     (os/generate-polyhedron
      (protocols/triangle-mesh outer-skin [100 100]))
     "circle(132.2, $fn=100);"
     #_(os/generate-polyhedron
        (protocols/triangle-mesh inner-skin [100 100])))))

(defn part-14 [right?]
  (let [start-cm   55
        end-cm     88
        interval-1 [(/ start-cm length-cm) (/ end-cm length-cm)]
        interval-2 (if right? [0 1/2] [1/2 1])
        side-shift 0 #_(if right? 50 50)
        outer-skin (protocols/linear-transform
                    (curves/cut-patch low-volume-kayak interval-1 #_interval-2)
                    (c/translate-matrix [side-shift 60 (- (* 10 start-cm))]))
        inner-skin (protocols/linear-transform
                    (curves/cut-patch low-volume-kayak-inside interval-1 #_interval-2)
                    (c/translate-matrix [side-shift 60 (- (* 10 start-cm))]))]
    (merge (gcode-layers/corrugated-panel-descriptor outer-skin inner-skin 60
                                                     (* layers-per-cm (- end-cm start-cm))
                                                     400)
           {:skirt-polyline (gcode/circular-polyline 130 (if right? Math/PI 0))})
    #_(os/write
     (os/generate-polyhedron
      (protocols/triangle-mesh outer-skin [100 100]))
     "circle(132.2, $fn=100);"
     #_(os/generate-polyhedron
        (protocols/triangle-mesh inner-skin [100 100])))))

(defn part-15 [right?]
  (let [start-cm   22
        end-cm     55
        interval-1 [(/ start-cm length-cm) (/ end-cm length-cm)]
        interval-2 (if right? [0 1/2] [1/2 1])
        side-shift 0 #_(if right? 50 50)
        outer-skin (protocols/linear-transform
                    (curves/cut-patch low-volume-kayak interval-1 #_interval-2)
                    (c/translate-matrix [side-shift 60 (- (* 10 start-cm))]))
        inner-skin (protocols/linear-transform
                    (curves/cut-patch low-volume-kayak-inside interval-1 #_interval-2)
                    (c/translate-matrix [side-shift 60 (- (* 10 start-cm))]))]
    #_(merge (gcode-layers/corrugated-panel-descriptor outer-skin inner-skin 60
                                                     (* layers-per-cm (- end-cm start-cm))
                                                     400)
           {:skirt-polyline (gcode/circular-polyline 130 (if right? Math/PI 0))})
    (os/write
     (os/generate-polyhedron
      (protocols/triangle-mesh outer-skin [100 100]))
     "circle(132.2, $fn=100);"
     #_(os/generate-polyhedron
        (protocols/triangle-mesh inner-skin [100 100])))))
