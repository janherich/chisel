(ns chisel.printer
  "Printer parts"
  (:require [chisel.protocols :as protocols]
            [chisel.curves :as curves]
            [chisel.coordinates :as c]
            [chisel.utils :as u]
            [chisel.conic-sections :as conics]
            [chisel.open-scad :as os]
            [chisel.gcode :as gcode]
            [chisel.gcode-layers :as gcode-layers]))

(def ^:private slider-length 80)
(def ^:private layers-per-mm 5)

(defn slider-panels [side]
  (let [inner-distance      (Math/sqrt (/ (Math/pow side 2) 2))
        outer-distance      (+ inner-distance 5)
        outer-left-point    (c/v [(- outer-distance) 0 0])
        outer-center-point  (c/v [0 outer-distance 0])
        outer-right-point   (c/v [outer-distance 0 0])
        inner-left-point    (c/v [(- inner-distance) 0 0])
        inner-center-point  (c/v [0 inner-distance 0])
        inner-right-point   (c/v [inner-distance 0 0])
        outer-left-curve    (curves/bezier-curve [outer-left-point
                                                  (u/lift-z outer-left-point slider-length)])
        outer-center-curve  (curves/bezier-curve [outer-center-point
                                                  (u/lift-z outer-center-point slider-length)])
        outer-right-curve   (curves/bezier-curve [outer-right-point
                                                  (u/lift-z outer-right-point slider-length)])
        inner-left-curve    (curves/bezier-curve [inner-left-point
                                                  (u/lift-z inner-left-point slider-length)])
        inner-center-curve  (curves/bezier-curve [inner-center-point
                                                  (u/lift-z inner-center-point slider-length)])
        inner-right-curve   (curves/bezier-curve [inner-right-point
                                                  (u/lift-z inner-right-point slider-length)])]
    [(curves/clamped-uniform-b-spline-patch
      {:control-curves [outer-left-curve outer-center-curve outer-right-curve]
       :order          1})
     (curves/clamped-uniform-b-spline-patch
      {:control-curves [inner-left-curve inner-center-curve inner-right-curve]
       :order          1})]))

(defn render-sliders []
  (let [[side-a side-b] (slider-panels 34)]
    (os/write
     (os/generate-polyhedron
      (protocols/triangle-mesh side-a [2 3]))
     (os/generate-polyhedron
      (protocols/triangle-mesh side-b [2 3])))))

(defn slider-panels-gcode []
  (let [[side-a side-b] (slider-panels 34)]
    (merge (gcode-layers/corrugated-panel-descriptor (protocols/linear-transform
                                                      side-a (c/translate-matrix [150 150 0]))
                                                     (protocols/linear-transform
                                                      side-b (c/translate-matrix [150 150 0]))
                                                     10
                                                     (* layers-per-mm slider-length)
                                                     3)
           {:skirt-polyline [[50 50] [250 50] [250 250] [50 250] [50 50]]})))

(comment
  (u/write-to-file
   "/Users/janherich/CAD/corner_panel.gcode"
   (gcode/generate-gcode (merge gcode/cr10-print-descriptor (slider-panels-gcode)))))

(def ^:private panel-length 500)

(defn corner-panels [radius]
  (let [half-radius         (/ radius 2)
        half-inner-radius   (- half-radius 10)
        outer-left-point    (c/v [(- half-radius) 0 0])
        outer-right-point   (c/v [half-radius 0 0])
        [outer-center-point
         center-point-w]    (conics/circle-arc-point outer-left-point outer-right-point 60)
        [inner-left-point
         inner-center-point
         inner-right-point] (u/distanced-path [outer-left-point
                                               outer-center-point
                                               outer-right-point]
                                              10)
        outer-left-curve    (curves/bezier-curve [outer-left-point
                                                  (u/lift-z outer-left-point panel-length)])
        outer-center-curve  (curves/bezier-curve [outer-center-point
                                                  (u/lift-z outer-center-point panel-length)])
        outer-right-curve   (curves/bezier-curve [outer-right-point
                                                  (u/lift-z outer-right-point panel-length)])
        inner-left-curve    (curves/bezier-curve [inner-left-point
                                                  (u/lift-z inner-left-point panel-length)])
        inner-center-curve  (curves/bezier-curve [inner-center-point
                                                  (u/lift-z inner-center-point panel-length)])
        inner-right-curve   (curves/bezier-curve [inner-right-point
                                                  (u/lift-z inner-right-point panel-length)])]
    [(curves/clamped-uniform-b-spline-patch
      {:control-curves [outer-left-curve
                        (with-meta outer-center-curve {:weight center-point-w})
                        outer-right-curve]
       :order          2})
     (curves/clamped-uniform-b-spline-patch
      {:control-curves [inner-left-curve
                        (with-meta inner-center-curve {:weight center-point-w})
                        inner-right-curve]
       :order          2})]))

(defn render-panels []
  (let [[side-a side-b] (corner-panels 300)]
    (os/write
     (os/generate-polyhedron
      (protocols/triangle-mesh side-a [2 100]))
     (os/generate-polyhedron
      (protocols/triangle-mesh side-b [2 100])))))

(defn corner-panels-gcode []
  (let [[side-a side-b] (corner-panels 300)]
    (merge (gcode-layers/corrugated-panel-descriptor side-a
                                                     side-b
                                                     20
                                                     (* layers-per-mm panel-length)
                                                     100)
           {:skirt-polyline (gcode/circular-polyline 180)})))

(defn effector-third [outer-edge inner-edge corner-clearence thickness]
  (let [half-thickness     (/ thickness 2)
        half-outer         (/ outer-edge 2)
        half-inner         (/ inner-edge 2)
        half-corner        (/ corner-clearence 2)
        right-lower-point  (c/v [half-outer half-thickness 0])
        right-middle-point (c/v [half-outer half-thickness half-corner])
        right-upper-point  (c/v [half-inner
                                 half-thickness
                                 (+ half-corner
                                    (* (Math/tan (Math/toRadians 30))
                                       (- half-outer half-inner)))])
        outer-right-curve (curves/clamped-uniform-b-spline
                           {:control-points [right-lower-point right-middle-point right-upper-point]
                            :order          1})
        outer-left-curve  (protocols/linear-transform outer-right-curve (c/flip-matrix :x))
        outer-patch       (curves/bezier-patch [outer-right-curve outer-left-curve])]
    (with-meta [outer-patch
                (protocols/linear-transform outer-patch (c/flip-matrix :y))]
      {:height (right-upper-point 2)})))

(defn render-effector []
  (let [[side-a side-b] (effector-third 60 10 20 6)]
    (os/write
     (os/generate-polyhedron
      (protocols/triangle-mesh side-a [200 2]))
     (os/generate-polyhedron
      (protocols/triangle-mesh side-b [200 2])))))

(defn effector-third-gcode []
  (let [[side-a side-b :as result] (effector-third 60 10 20 6)]
    (merge (gcode-layers/corrugated-panel-descriptor (protocols/linear-transform
                                                      side-a (c/translate-matrix [150 150 0]))
                                                     (protocols/linear-transform
                                                      side-b (c/translate-matrix [150 150 0]))
                                                     5
                                                     (int (* layers-per-mm (:height (meta result))))
                                                     2)
           {:skirt-polyline [[50 50] [250 50] [250 250] [50 250] [50 50]]})))
