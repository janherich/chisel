(ns chisel.linear-slides
  "Linear slide implementations"
  (:require [chisel.protocols :as protocols]
            [chisel.curves :as curves]
            [chisel.conic-sections :as conics]
            [chisel.coordinates :as c]
            [chisel.utils :as u]
            [chisel.open-scad :as os]
            [chisel.gcode :as gcode]
            [chisel.gcode-layers :as gcode-layers]))

(def ^:private layers-per-cm 40)

(defn wedge-sliding-carriage
  "Generates wedge sliding carriage with given width (between nearest edges), thickness and height.
  Optionally accepts wedge angle in degrees (defaults to right angle) to generate carriage for non-square rails 
  and/or preloaded fit where preload force will align bearing faces to sit square on guide rails."
  ([width thickness height]
   (wedge-sliding-carriage width thickness height 90))
  ([width thickness height wedge-angle]
   (let [half-width              (/ width 2)
         half-thickness          (/ thickness 2)
         bearing-face-x-distance (* half-thickness (Math/tan (Math/toRadians (/ wedge-angle 2))))
         center-edge-distance    (+ half-width bearing-face-x-distance)
         valley-curve            (curves/bezier-curve [(c/v [(- half-width) 0 0])
                                                       (c/v [(- half-width) 0 height])])
         edge-curve              (curves/bezier-curve [(c/v [(- center-edge-distance) half-thickness 0])
                                                       (c/v [(- center-edge-distance) half-thickness height])])
         face-1 (curves/clamped-uniform-b-spline-patch
                 {:control-curves [valley-curve
                                   edge-curve
                                   (protocols/linear-transform edge-curve (c/flip-matrix :x))
                                   (protocols/linear-transform valley-curve (c/flip-matrix :x))]
                  :order          1})
         face-2 (protocols/linear-transform face-1 (c/flip-matrix :y))
         bearing-face (u/hypotenuse half-thickness bearing-face-x-distance)
         face-curve-length (+ width (* 2 (+ bearing-face bearing-face-x-distance)))]
     (gcode-layers/corrugated-panel-descriptor face-1 face-2 (int (Math/ceil (/ width 10)))
                                               (* layers-per-cm (/ height 10))
                                               4
                                               :corrugate-fn
                                               (partial gcode-layers/cutoff-corrugations
                                                        (/ (+ bearing-face bearing-face-x-distance)
                                                           face-curve-length)))
     #_(os/write
        (os/generate-polyhedron
         (protocols/triangle-mesh face-1 [2 4]))
        (os/generate-polyhedron
         (protocols/triangle-mesh face-2 [2 4]))))))

(comment
  (u/write-to-file
   "/Users/janherich/CAD/sliding_carriage.gcode"
   (gcode/generate-gcode (merge gcode/qqs-print-descriptor
                                (wedge-sliding-carriage 43 20 50 86)))))

(defn arc-sliding-carriage
  "Generates arc sliding carriage with given width (between nearest points on both arcs) an arc radius.
  Optionally accepts arc degrees argument, which determines portion of the arc the bearing face will cover,
  defaults to 120."
  ([width arc-radius height]
   (arc-sliding-carriage width arc-radius height 120))
  ([width arc-radius height arc-degrees]
   (let [half-width             (/ width 2)
         half-angle             (Math/toRadians (/ arc-degrees 2))
         half-thickness         (* arc-radius (Math/sin half-angle))
         extra-width            (- arc-radius (* arc-radius (Math/cos half-angle)))
         center-edge-distance   (+ half-width extra-width)
         [arc-point arc-weight] (conics/circle-arc-point (c/v [(- half-width) 0])
                                                         (c/v [(- center-edge-distance) half-thickness])
                                                         (/ arc-degrees 2)
                                                         :counterclockwise? true)
         valley-curve           (curves/bezier-curve [(c/v [(- half-width) 0 0])
                                                      (c/v [(- half-width) 0 height])])
         arc-point-curve        (curves/bezier-curve [arc-point
                                                      (protocols/linear-transform arc-point (c/translate-matrix [0 0 height]))])
         edge-curve             (curves/bezier-curve [(c/v [(- center-edge-distance) half-thickness 0])
                                                      (c/v [(- center-edge-distance) half-thickness height])])
         center-curve           (curves/bezier-curve [(c/v [0 0 0])
                                                      (c/v [0 0 height])])
         face-1                 (curves/clamped-b-spline-patch
                                 {:control-curves [valley-curve
                                                   (with-meta arc-point-curve {:weight arc-weight})
                                                   edge-curve
                                                   center-curve
                                                   (protocols/linear-transform edge-curve (c/flip-matrix :x))
                                                   (with-meta
                                                     (protocols/linear-transform arc-point-curve (c/flip-matrix :x))
                                                     {:weight arc-weight})
                                                   (protocols/linear-transform valley-curve (c/flip-matrix :x))]
                                  :knot-vector   [1/3 1/3 2/3 2/3]
                                  :order 2})
         face-2                 (protocols/linear-transform face-1 (c/flip-matrix :y))
         bearing-face           (* half-angle arc-radius)
         face-curve-length      (+ width (* 2 (+ bearing-face extra-width)))]
     (gcode-layers/corrugated-panel-descriptor face-1 face-2 (int (Math/ceil (/ width 10)))
                                               (* layers-per-cm (/ height 10))
                                               100
                                               :corrugate-fn
                                               (partial gcode-layers/cutoff-corrugations
                                                        (/ (+ bearing-face extra-width)
                                                           (* 9/10 face-curve-length))))
     #_(os/write
      (os/generate-polyhedron
       (protocols/triangle-mesh face-1 [2 100]))
      (os/generate-polyhedron
       (protocols/triangle-mesh face-2 [2 100]))))))

(comment
  (u/write-to-file
   "/Users/janherich/CAD/arc_sliding_carriage.gcode"
   (gcode/generate-gcode (merge gcode/qqs-print-descriptor
                                (arc-sliding-carriage 55 13 50 100)))))

(defn clamping-arc-sliding-carriage
  "Generates clamping arc sliding carriage with given rails spacing and rails radius and bearing-angle"
  ([rails-spacing rails-radius height]
   (clamping-arc-sliding-carriage rails-spacing rails-radius height 120))
  ([rails-spacing rails-radius height bearing-angle]
   (let [wall-thickness           3/2
         angle-rad                (Math/toRadians bearing-angle)
         half-spacing             (/ rails-spacing 2)
         half-angle               (/ angle-rad 2)
         quarter-angle            (/ half-angle 2)
         y-offset                 (* rails-radius (Math/cos half-angle))
         x-offset                 (* rails-radius (Math/sin half-angle))
         central-point            (c/v [(- half-spacing) 0])
         bearing-end              (protocols/linear-transform
                                   central-point (c/translate-matrix [(- x-offset) y-offset 0]))
         bearing-start            (protocols/linear-transform
                                   central-point (c/translate-matrix [x-offset y-offset 0]))
         [arc-point arc-weight]   (conics/circle-arc-point bearing-end bearing-start bearing-angle)
         wall-end                 (protocols/linear-transform
                                   central-point (c/translate-matrix [(* wall-thickness (- x-offset))
                                                                      (* wall-thickness y-offset)
                                                                      0]))
         wall-start-y             (* rails-radius wall-thickness (Math/cos quarter-angle))
         wall-start-x             (* rails-radius wall-thickness (Math/sin quarter-angle))
         wall-start               (protocols/linear-transform central-point (c/translate-matrix [wall-start-x wall-start-y 0]))
         [wall-point wall-weight] (conics/circle-arc-point wall-end wall-start (* 3/4 bearing-angle))
         bearing-start-curve      (curves/bezier-curve [bearing-start (protocols/linear-transform bearing-start (c/translate-matrix [0 0 height]))])
         bearing-arc-curve        (curves/bezier-curve [arc-point (protocols/linear-transform arc-point (c/translate-matrix [0 0 height]))])
         bearing-end-curve        (curves/bezier-curve [bearing-end (protocols/linear-transform bearing-end (c/translate-matrix [0 0 height]))])
         wall-start-curve         (curves/bezier-curve [wall-start (protocols/linear-transform wall-start (c/translate-matrix [0 0 height]))])
         wall-arc-curve           (curves/bezier-curve [wall-point (protocols/linear-transform wall-point (c/translate-matrix [0 0 height]))])
         wall-end-curve           (curves/bezier-curve [wall-end (protocols/linear-transform wall-end (c/translate-matrix [0 0 height]))])
         face-1                   (curves/clamped-b-spline-patch
                                   {:control-curves [wall-end-curve
                                                     (with-meta wall-arc-curve {:weight wall-weight})
                                                     wall-start-curve
                                                     (protocols/linear-transform wall-start-curve (c/flip-matrix :x))
                                                     (with-meta
                                                       (protocols/linear-transform wall-arc-curve (c/flip-matrix :x))
                                                       {:weight wall-weight})
                                                     (protocols/linear-transform wall-end-curve (c/flip-matrix :x))]
                                    :knot-vector   [1/2 1/2 1/2]
                                    :order 2})
         face-2                   (curves/clamped-b-spline-patch
                                   {:control-curves [bearing-end-curve
                                                     (with-meta bearing-arc-curve {:weight arc-weight})
                                                     bearing-start-curve
                                                     (protocols/linear-transform bearing-start-curve (c/flip-matrix :x))
                                                     (with-meta
                                                       (protocols/linear-transform bearing-arc-curve (c/flip-matrix :x))
                                                       {:weight arc-weight})
                                                     (protocols/linear-transform bearing-end-curve (c/flip-matrix :x))]
                                    :knot-vector   [1/2 1/2 1/2]
                                    :order 2})]
     #_(os/write
      (os/generate-polyhedron
       (protocols/triangle-mesh face-1 [2 100]))
      (os/generate-polyhedron
       (protocols/triangle-mesh face-2 [2 100])))
     (gcode-layers/corrugated-panel-descriptor face-1 face-2 20 (* layers-per-cm (/ height 10)) 100))))

(comment
  (u/write-to-file
   "/Users/janherich/CAD/clamping_arc_sliding_carriage.gcode"
   (gcode/generate-gcode (merge gcode/qqs-print-descriptor
                                (clamping-arc-sliding-carriage 80.1 17.7 50)))))
