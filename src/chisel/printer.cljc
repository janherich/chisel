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

;; Effector sandwich core

(def ^:private layers-per-mm 5)

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
  (let [[side-a side-b] (effector-third 60 10 30 6)]
    (os/write
     (os/generate-polyhedron
      (protocols/triangle-mesh side-a [200 2]))
     (os/generate-polyhedron
      (protocols/triangle-mesh side-b [200 2])))))

(defn effector-third-gcode []
  (let [[side-a side-b :as result] (effector-third 60 10 30 6)]
    (merge (gcode-layers/corrugated-panel-descriptor (protocols/linear-transform
                                                      side-a (c/translate-matrix [150 150 0]))
                                                     (protocols/linear-transform
                                                      side-b (c/translate-matrix [150 150 0]))
                                                     5
                                                     (int (* layers-per-mm (:height (meta result))))
                                                     2)
           {:skirt-polyline [[50 50] [250 50] [250 250] [50 250] [50 50]]})))

;; Tubular delta utilities

(defn tubes
  "Calculates length of tubing necessary for delta with given dimensions"
  [h w]
  (+ (* 3 h)
     (* 6 w)
     (* 3 (Math/sqrt (+ (Math/pow h 2) (Math/pow w 2))))))

(defn corner-tube-length
  "Given base tube length, bed-diameter, sphere, end, beam & effector offsets, calculates
  necessary corner tube length"
  [base-tube-length bed-diameter sphere-offset end-offset beam-offset effector-offset]
  (let [triangle-side       (+ base-tube-length (* 2 sphere-offset))
        bed-radius          (/ bed-diameter 2)
        outcircle-radius    (/ triangle-side (Math/sqrt 3))
        incircle-radius     (/ outcircle-radius 2)
        corner-rod-distance (- outcircle-radius bed-radius effector-offset beam-offset)
        corner-length       (* 2 (* (Math/tan (Math/toRadians 30)) corner-rod-distance))]
    (- corner-length (* 2 end-offset))))

(comment
  (corner-rod-length 1000 500 43.1563 48.7333 72.2 50)) ;; 197

;; Tubular delta parametric parts

(def ^:private bolts-spec {:m8 {:hole-radius 4.2
                                :nut-radius  7.7}
                           :m5 {:hole-radius        2.7
                                :insert-hole-radius 3.2
                                :nut-radius         4.7}
                           :m4 {:hole-radius        2.2
                                :insert-hole-radius 2.8
                                :nut-radius         3.95}
                           :m3 {:hole-radius        1.7
                                :insert-hole-radius 2.0
                                :nut-radius         3.2}})

(def ^:private idler-channel-width 10)
(def ^:private belt-beam-offset 7)
(def ^:private tube-hole-depth 1)
(def ^:private angle-45 (Math/toRadians 45))
(def ^:private angle-60 (Math/toRadians 60))
(def ^:private align-hole-r 0.8)

(defn- beam-slider-width [beam-side]
  (+ beam-side 11))

(defn slider-panels
  "Returns tuble of patches for slider-panels"
  [side slider-length]
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

(defn slider-panel-gcode
  "Generates gcode for rectangular slider panels"
  [beam-size]
  (let [[side-a side-b] (slider-panels (+ beam-size 2) (* 2 beam-size))]
    (merge (gcode-layers/corrugated-panel side-b
                                          side-a
                                          :corrugate-fn     gcode-layers/sine-corrugations
                                          :skin-resolution  3
                                          :corrugation-size 5
                                          :core-line-width  4/10
                                          :skin-line-width  6/10))))

;; Sliders for 40mm beam
(comment
  (u/write-to-file
   "/Users/janherich/CAD/corner_panel_40.gcode"
   (gcode/generate-gcode (merge gcode/deltav1-print-descriptor
                                gcode/ca-pet-print-descriptor
                                (slider-panels-gcode 40)))))

;; Sliders for 30mm beam
(comment
  (u/write-to-file
   "/Users/janherich/CAD/corner_panel_30.gcode"
   (gcode/generate-gcode (merge gcode/deltav1-print-descriptor
                                gcode/ca-pet-print-descriptor
                                (slider-panels-gcode 30)))))

(defn hole-depth
  "Calculates hole depth for given tube radius, sphere radius and diagonal-angle"
  [sphere-radius tube-radius diagonal-angle]
  (let [sin-radius         (Math/sin diagonal-angle)
        cos-radius         (Math/cos diagonal-angle)
        horizontal-cut     (+ tube-radius (* sphere-radius sin-radius))
        vertical-cut       (* sphere-radius cos-radius)]
    (- vertical-cut (Math/sqrt (- (Math/pow sphere-radius 2)
                                  (Math/pow horizontal-cut 2))))))

(defn- tube-joint [tube-r sphere-r offset wall-thickness bolt-r]
  (os/union
   (os/translate {:z offset} (os/cylinder {:radius tube-r
                                           :height tube-r}))
   (os/cylinder {:radius bolt-r
                 :height sphere-r})
   (os/cylinder {:radius (* 2 bolt-r)
                 :height (- sphere-r wall-thickness)})))

(defn- insert-hole
  "Hole for press inserts"
  [radius]
  (os/cylinder {:radius radius
                :height (* 3 radius)}))

(defn- cover-insert-hole [sphere-r press-hole-r]
  (os/translate {:y (* 0.8 (- sphere-r)) :z (* 0.2 sphere-r)} (insert-hole press-hole-r)))

(defn- calculate-tube-offset [sphere-r tube-r tube-hole-depth]
  (- (Math/sqrt (- (Math/pow sphere-r 2) (Math/pow tube-r 2))) tube-hole-depth))

(defn hemispherical-corner-piece
  "Returns OpenSCAD string object for hemispherical corner piece of tubular delta.
  Takes sphere-diameter, tube-diameter, horizontal/vertical tube length and bolt/insert params."
  [{:keys [sphere-diameter tube-diameter
           horizontal-tube-length vertical-tube-length
           bolt press-insert]}]
  (let [bolt-r              (get-in bolts-spec [bolt :hole-radius])
        press-hole-r        (get-in bolts-spec [press-insert :insert-hole-radius])
        tube-r              (/ tube-diameter 2)
        wall-thickness      (/ tube-diameter 2)
        sphere-r            (/ sphere-diameter 2)
        tube-offset         (calculate-tube-offset sphere-r tube-r tube-hole-depth)
        height              (+ horizontal-tube-length (* 2 tube-offset))
        width               (+ vertical-tube-length (* 2 tube-offset))
        diagonal-offset     (* sphere-r (Math/sin angle-45))
        diagonal-height     (- height (* 2 diagonal-offset))
        diagonal-width      (- width (* 2 diagonal-offset))
        diagonal-angle      (Math/atan (/ diagonal-height diagonal-width))
        angle-diff          (- angle-45 diagonal-angle)
        diagonal-hole-depth (+ (hole-depth sphere-r tube-r angle-diff) 0.4)
        diagonal-length     (+ (Math/sqrt (+ (Math/pow diagonal-width 2)
                                             (Math/pow diagonal-height 2)))
                               (* 2 diagonal-hole-depth))]
    (os/difference
     (os/rotate {:x -45}
                (os/difference
                 (os/sphere {:radius sphere-r})
                 (os/sphere {:radius (- sphere-r wall-thickness)})
                 ;; hole for vertical tube
                 (tube-joint tube-r sphere-r tube-offset wall-thickness bolt-r)
                 ;; holes for horizontal tubes
                 (os/rotate {:x 90 :z 30}
                            (tube-joint tube-r sphere-r tube-offset wall-thickness bolt-r))
                 (os/rotate {:x 90 :z -30}
                            (tube-joint tube-r sphere-r tube-offset wall-thickness bolt-r))
                 ;; hole for diagonal tube
                 (os/rotate
                  {:x 45 :z 30}
                  (os/translate {:z sphere-r}
                                (os/rotate
                                 {:x (- (Math/toDegrees angle-diff))}
                                 (os/translate {:z (- diagonal-hole-depth)}
                                               (os/cylinder {:radius tube-r
                                                             :height (+ tube-diameter
                                                                        diagonal-hole-depth)}))
                                 (os/translate {:z (- (/ sphere-r 2))}
                                               (os/cylinder {:radius bolt-r
                                                             :height sphere-r}))
                                 (os/translate {:z (- (+ diagonal-hole-depth wall-thickness))}
                                               (os/cylinder {:radius (* 2 bolt-r)
                                                             :height (/ wall-thickness 2)})))))))
     (os/translate {:z (- sphere-r)}
                   (os/cylinder {:radius (+ 1 sphere-r)
                                 :height (* 1.2 sphere-r)}))
     (cover-insert-hole sphere-r press-hole-r)
     (os/rotate {:z 120} (cover-insert-hole sphere-r press-hole-r))
     (os/rotate {:z -120} (cover-insert-hole sphere-r press-hole-r)))))

(comment
  (os/write
   (hemispherical-corner-piece
    {:sphere-diameter        59
     :tube-diameter          20.3
     :horizontal-tube-length 600
     :vertical-tube-length   925
     :bolt                   :m5
     :press-insert           :m3})))

(defn- rounded-cylinder [{:keys [radius height rounding-radius rounding]
                          :or   {rounding-radius (/ radius 10)
                                 rounding :top}}]
  (let [straight-x (- radius rounding-radius)
        straight-y (- height rounding-radius)]
    (os/rotate-extrude
     (os/square [straight-x height])
     (os/translate (if (= :top rounding)
                     {:x straight-x :y straight-y}
                     {:x straight-x :y rounding-radius})
                   (os/circle {:radius rounding-radius}))
     (os/translate (when (= :bottom rounding) {:y rounding-radius})
                   (os/square [radius straight-y])))))

(defn- double-rounded-cylinder [{:keys [radius height rounding-radius]
                                 :or   {rounding-radius (/ radius 10)}}]
  (let [straight-x (- radius rounding-radius)]
    (os/rotate-extrude
     (os/square [straight-x height])
     (os/translate {:x straight-x :y (- height rounding-radius)}
                   (os/circle {:radius rounding-radius}))
     (os/translate {:x straight-x :y rounding-radius} (os/circle {:radius rounding-radius}))
     (os/translate {:x straight-x :y rounding-radius}
                   (os/square [rounding-radius (- height (* rounding-radius 2))])))))

(defn- ball-flower-height [ball-diameter]
  (* ball-diameter 3/4))

(defn- ball-flower-diameter [ball-diameter]
  (+ ball-diameter 2.2))

(defn ball-flower [{:keys [ball-diameter cuts]
                    :or   {cuts 6}}]
  (let [ball-r    (/ ball-diameter 2)
        outer-r   (/ (ball-flower-diameter ball-diameter) 2)
        height    (ball-flower-height ball-diameter)
        cut-angle (/ 360 cuts)]
    (os/difference
     (rounded-cylinder {:radius outer-r :height height :rounding :bottom})
     (os/translate {:z (+ ball-r 4/10)}
                   (os/sphere {:radius ball-r}))
     (apply os/translate {:z 2}
            (for [i (range cuts)]
              (os/rotate {:z (* i cut-angle)}
                         (os/hull (os/cylinder {:radius 2/10 :height (- height 2)})
                                  (os/translate {:x outer-r}
                                                (os/cylinder {:radius 2/10 :height (- height 2)})))))))))

(comment
  (os/write
   (ball-flower {:ball-diameter 12.1})))

(defn- effector-bearing-shell [sphere-r bearing-shell-outer-r flower-height right?]
  (os/rotate {:z (if right? 120 240) :x 30}
             (os/translate {:y (- (* bearing-shell-outer-r 5/6))
                            :z -7}
                           (rounded-cylinder {:radius bearing-shell-outer-r
                                              :height (+ flower-height 10)}))))

(defn- effector-bearing-pocket [bearing-shell-outer-r bearing-shell-inner-r flower-height right?]
  (os/rotate {:z (if right? 120 240) :x 30}
             (os/translate {:y (- (* bearing-shell-outer-r 5/6))
                            :z 3}
                           (os/cylinder {:radius bearing-shell-inner-r
                                         :height flower-height}))
             (os/translate {:y (- (* bearing-shell-outer-r 5/6))
                            :z -7}
                           (os/cylinder {:radius (/ bearing-shell-inner-r 2)
                                         :height 10}))))

(defn- aligning-holes [sphere-r bearing-shell-outer-r]
  (let [tapered-cylinder (os/union
                          (os/cylinder {:radius        (* align-hole-r 4/3)
                                        :second-radius align-hole-r
                                        :height 1})
                          (os/translate {:z 1} (os/cylinder {:radius align-hole-r
                                                             :height 3})))]
    (os/translate {:z (- (* sphere-r 2/3))}
                  (os/translate {:y (- (* sphere-r 1/3))} tapered-cylinder)
                  (os/translate {:y (* sphere-r 2/3)} tapered-cylinder))))

(defn effector-node
  "Returns OpenSCAD string object for effector piece of tubular delta.
  Takes sphere-diameter, tube-diameter and bearing-diameter parameters"
  [{:keys [sphere-diameter tube-diameter ball-diameter]}]
  (let [sphere-r              (/ sphere-diameter 2)
        tube-r                (/ tube-diameter 2)
        tube-offset           (calculate-tube-offset sphere-r tube-r 4.2)
        bearing-shell-inner-r (/ (ball-flower-diameter ball-diameter) 2)
        bearing-shell-outer-r (+ bearing-shell-inner-r 1)
        flower-height         (ball-flower-height ball-diameter)]
    (println (format "Bearing offset: %s" tube-offset))
    (os/difference
     (os/union
      (os/sphere {:radius sphere-r})
      ;; right bearing shell
      (effector-bearing-shell sphere-r bearing-shell-outer-r flower-height true)
      ;; left bearing shell
      (effector-bearing-shell sphere-r bearing-shell-outer-r flower-height false))
     ;; right bearing pocket
     (effector-bearing-pocket bearing-shell-outer-r bearing-shell-inner-r flower-height true)
     ;; left bearing pocket
     (effector-bearing-pocket bearing-shell-outer-r bearing-shell-inner-r flower-height false)
     ;; aligning holes
     (aligning-holes sphere-r bearing-shell-outer-r)
     ;; hole for horizontal tubes
     (os/rotate {:x 90 :z 30}
                (os/translate {:z tube-offset} (os/cylinder {:radius tube-r :height sphere-r})))
     ;; hole for horizontal tubes
     (os/rotate {:x 90 :z -30}
                (os/translate {:z tube-offset} (os/cylinder {:radius tube-r :height sphere-r})))
     ;; hole for diagonal tube
     (os/rotate {:x 25}
                (os/translate {:z tube-offset} (os/cylinder {:radius tube-r :height sphere-r})))
     ;; cutout for flat bottom to promote bed adhesion
     (os/translate {:z -50}
                   (os/cylinder {:radius 20 :height (- 50 (* sphere-r 2/3))})))))

(comment
  "Settings for Delta V1 printer"
  (os/write
   (effector-node
    {:sphere-diameter 20
     :tube-diameter   6.3
     :ball-diameter   12.3})))

(defn effector-node-jig
  "Returns jig for effector node, takes sphere-diameter, tube-diameter and rod-spacing as params"
  [{:keys [sphere-diameter tube-diameter rod-spacing]}]
  (let [sphere-r            (/ sphere-diameter 2)
        center-distance     (/ (/ rod-spacing 2) (Math/sin angle-60))
        outer-hole-distance (+ center-distance (* 2/3 sphere-r))
        inner-hole-distance (- center-distance (* 1/3 sphere-r))]
    (os/difference
     (os/union
      (os/linear-extrude {:height 6}
                         (os/text {:text   (format "Rod spacing: %s" rod-spacing)
                                   :size   3
                                   :halign "center"}))
      (os/hull
       (os/rotate {:z 60} (os/translate {:x center-distance} (os/cylinder {:radius sphere-diameter
                                                                           :height 5})))
       (os/rotate {:z -60} (os/translate {:x center-distance} (os/cylinder {:radius sphere-diameter
                                                                            :height 5})))
       (os/rotate {:z 180} (os/translate {:x center-distance} (os/cylinder {:radius sphere-diameter
                                                                            :height 5})))))
     (os/rotate {:z 60}
                (os/translate {:x outer-hole-distance} (os/cylinder {:radius align-hole-r
                                                                     :height 5}))
                (os/translate {:x inner-hole-distance} (os/cylinder {:radius align-hole-r
                                                                     :height 5})))
     (os/rotate {:z -60}
                (os/translate {:x outer-hole-distance} (os/cylinder {:radius align-hole-r
                                                                     :height 5}))
                (os/translate {:x inner-hole-distance} (os/cylinder {:radius align-hole-r
                                                                     :height 5})))
     (os/rotate {:z 180}
                (os/translate {:x outer-hole-distance} (os/cylinder {:radius align-hole-r
                                                                     :height 5}))
                (os/translate {:x inner-hole-distance} (os/cylinder {:radius align-hole-r
                                                                     :height 5}))))))

(comment
  (os/write
   (effector-node-jig {:sphere-diameter 20
                       :tube-diameter   6.3
                       :rod-spacing     57.5})))

(defn- carriage-bearing-shell [bearing-shell-outer-r flower-height]   
  (os/rotate {:x -30}
             (os/translate {:y (* bearing-shell-outer-r 4/5)
                            :z -1}
                           (rounded-cylinder {:radius bearing-shell-outer-r
                                              :height (+ flower-height 10)}))))

(defn- carriage-bearing-pocket [bearing-shell-outer-r bearing-shell-inner-r flower-height]
  (os/rotate {:x -30}
             (os/translate {:y (* bearing-shell-outer-r 4/5)
                            :z 9}
                           (os/cylinder {:radius bearing-shell-inner-r
                                         :height flower-height}))
             (os/translate {:y (* bearing-shell-outer-r 4/5)
                            :z -1}
                           (os/cylinder {:radius (/ bearing-shell-inner-r 2)
                                         :height 10}))))

(defn carriage-adapter
  "Returns OpenSCAD string object for carriage adapter to be glued on rectangular beam slider.
  Takes ball-diameter, and beam-side as params"
  [{:keys [ball-diameter beam-side]}]
  (let [beam-cutout           (beam-slider-width beam-side)
        bearing-shell-inner-r (/ (ball-flower-diameter ball-diameter) 2)
        bearing-shell-outer-r (+ bearing-shell-inner-r 1)
        flower-height         (ball-flower-height ball-diameter)
        rod-spacing           (+ beam-cutout (* 2 bearing-shell-outer-r))]
    (println (format "Rod spacing: %s" rod-spacing))
    (os/difference
     (os/union
      (os/hull
       (os/translate {:x (/ rod-spacing 2)}
                     (double-rounded-cylinder {:radius 3 :height 3}))
       (os/translate {:x (- (/ rod-spacing 2))}
                     (double-rounded-cylinder {:radius 3 :height 3}))
       (os/translate {:x (/ rod-spacing 2)
                      :y (* 3/2 bearing-shell-outer-r)}
                     (double-rounded-cylinder {:radius 3 :height 3}))
       (os/translate {:x (- (/ rod-spacing 2))
                      :y (* 3/2 bearing-shell-outer-r)}
                     (double-rounded-cylinder {:radius 3 :height 3})))
      (os/translate {:y (+ bearing-shell-outer-r 1)}
       (os/hull
        (os/translate {:x (/ rod-spacing 2)}
                      (rounded-cylinder {:radius 1
                                         :height 10
                                         :rounding-radius 1/2}))
        (os/translate {:x (- (/ rod-spacing 2))}
                      (rounded-cylinder {:radius 1
                                         :height 10
                                         :rounding-radius 1/2}))))
      (os/translate {:x (/ rod-spacing 2)}
                    (carriage-bearing-shell bearing-shell-outer-r flower-height))
      (os/translate {:x (- (/ rod-spacing 2))}
                    (carriage-bearing-shell bearing-shell-outer-r flower-height)))
     (os/cube [beam-cutout (* 2 bearing-shell-outer-r) 6] :center true)
     (os/translate {:x (/ rod-spacing 2)}
                   (carriage-bearing-pocket bearing-shell-outer-r
                                            bearing-shell-inner-r
                                            flower-height))
     (os/translate {:x (- (/ rod-spacing 2))}
                   (carriage-bearing-pocket bearing-shell-outer-r
                                            bearing-shell-inner-r
                                            flower-height))
     (os/translate {:z (- (* 2 bearing-shell-outer-r))}
                   (os/cylinder {:radius (+ rod-spacing bearing-shell-outer-r)
                                 :height (* 2 bearing-shell-outer-r)})))))

(comment
  (os/write
   (carriage-adapter {:ball-diameter 12.3
                      :beam-side     30})))

(defn- cover-bolt-hole [sphere-r hole-r]
  (os/translate {:y (* 0.8 (- sphere-r))}
                (os/cylinder {:radius hole-r
                              :height (* 4 hole-r)})
                (os/translate {:z (* 2 hole-r)}
                              (os/cylinder {:radius (* 2 hole-r)
                                            :height (* 2 hole-r)}))))

(defn hemispherical-cover
  "Returns OpenSCAD string object for hemispherical cover piece of tubular delta.
  Takes sphere-diameter and bolt params"
  [{:keys [sphere-diameter bolt]}]
  (let [sphere-r    (/ sphere-diameter 2)
        effective-r (Math/sqrt (- (Math/pow sphere-r 2) (Math/pow (* sphere-r 0.2) 2)))
        bolt-hole-r (get-in bolts-spec [bolt :hole-radius])
        height      (* 4 bolt-hole-r)]
    (os/difference
     (os/union
      (os/linear-extrude {:height (+ 1 height)}
                         (os/text {:text   "T"
                                   :size   (/ sphere-diameter 2)
                                   :valign "center"
                                   :halign "center"}))
      (rounded-cylinder {:radius          effective-r
                         :height          height
                         :rounding-radius bolt-hole-r}))
     (cover-bolt-hole sphere-r bolt-hole-r)
     (os/rotate {:z 120} (cover-bolt-hole sphere-r bolt-hole-r))
     (os/rotate {:z -120} (cover-bolt-hole sphere-r bolt-hole-r)))))

(comment
  (os/write
   (hemispherical-cover
    {:sphere-diameter 59
     :bolt            :m3})))


(defn nut-spacer
  "Returns OpenSCAD string object for nut-spacer. Takes diameter and bolt params"
  [{:keys [diameter nut]}]
  (let [outer-r (/ diameter 2)
        nut-r   (get-in bolts-spec [nut :nut-radius])]
    (os/difference
     (double-rounded-cylinder {:radius outer-r :height outer-r})
     (os/cylinder {:radius (* 2/3 nut-r)
                   :height outer-r})
     (os/translate {:z (/ outer-r 2)}
                   (os/cylinder {:radius     nut-r
                                 :height     (/ outer-r 2)
                                 :resolution 6})))))

(comment
  (os/write
   (nut-spacer {:diameter 18.4
                :nut      :m5})))

(defn belt-catch
  "Returns OpenSCAD string object for belt-catch. Takes length and optional belt-height params"
  [{:keys [length belt-height] :or {belt-height 6}}]
  (let [belt-notch-depth (+ belt-height 1)
        belt-offset      (- belt-beam-offset 5)
        body-radius      6
        half-length      (/ length 2)]
    (os/difference
     ;; main body
     (os/hull
      (os/translate {:x (- half-length body-radius)}
                    (rounded-cylinder {:radius body-radius
                                       :height (+ belt-offset belt-notch-depth)}))
      (os/translate {:x (- (- half-length body-radius))}
                    (rounded-cylinder {:radius body-radius
                                       :height (+ belt-offset belt-notch-depth)})))
     ;; symmetric eyelets
     (os/translate {:x (- half-length 17) :z belt-offset}
                   (os/difference
                    (os/scale {:x 2}
                              (os/cylinder {:radius 4 :height belt-notch-depth}))
                    (os/scale {:x 2.2}
                              (os/cylinder {:radius 2.1 :height belt-notch-depth}))))
     (os/translate {:x (- (- half-length 17)) :z belt-offset}
                   (os/difference
                    (os/scale {:x 2}
                              (os/cylinder {:radius 4 :height belt-notch-depth}))
                    (os/scale {:x 2.2}
                              (os/cylinder {:radius 2.1 :height belt-notch-depth}))))
     ;; symmetric belt entries
     (os/hull
      (os/translate {:x (- half-length 11) :z belt-offset}
                    (os/cylinder {:radius 1.3 :height belt-notch-depth}))
      (os/translate {:x (+ half-length 1) :z belt-offset}
                    (os/cylinder {:radius 1.15 :height belt-notch-depth})))
     (os/hull
      (os/translate {:x (- (- half-length 11)) :z belt-offset}
                    (os/cylinder {:radius 1.3 :height belt-notch-depth}))
      (os/translate {:x (- (+ half-length 1)) :z belt-offset}
                    (os/cylinder {:radius 1.15 :height belt-notch-depth})))
     ;; central lightening hole
     (os/hull
      (os/translate {:x (max 0 (- half-length 30)) :z belt-offset}
                    (os/cylinder {:radius 4 :height belt-notch-depth}))
      (os/translate {:x (min 0 (- (- half-length 30))) :z belt-offset}
                    (os/cylinder {:radius 4 :height belt-notch-depth}))))))

(comment
  (os/write
   (belt-catch {:length 60})))

(defn- tube-clamping-part-outer-r [tube-radius] (* 1.8 tube-radius))

(defn- tube-clamping-part-bolt-offset [tube-radius outer-r]
  (+ tube-radius (/ (- outer-r tube-radius) 2)))

(defn- corner-end-length [tube-diameter] (* tube-diameter 1.5))

(defn- chamfered-cylinder [radius height]
  (os/union
   (os/cylinder {:radius radius :height height})
   ;; chamfer to combat elephant foot effect
   (os/cylinder {:radius        (* radius 1.05)
                 :second-radius radius
                 :height        (/ radius 10)})))

(defn- tube-clamping-part-rounding-radius [outer-r] (/ outer-r 8))

(defn corner-end
  "Returns OpenSCAD string object for delta printer corner end. 
  Takes tube-diameter, bolt and press-fit params."
  [{:keys [tube-diameter bolt press-fit]}]
  (let [length             (corner-end-length tube-diameter)
        inner-r            (/ tube-diameter 2)
        outer-r            (tube-clamping-part-outer-r inner-r)
        clamping-offset    (tube-clamping-part-bolt-offset inner-r outer-r)
        hole-depth         (/ tube-diameter 4)
        centerline-offset  (- length hole-depth)
        bolt-radius        (get-in bolts-spec [bolt :hole-radius])
        insert-hole-radius (get-in bolts-spec [press-fit :insert-hole-radius])]
    (println (format "Centerline offset: %s" centerline-offset))
    (os/difference
     (os/hull
      (os/scale {:y 8/10}
                (double-rounded-cylinder {:radius          outer-r
                                          :height          length
                                          :rounding-radius (/ outer-r 8)}))
      (os/translate
       {:y (- (* outer-r (Math/sin angle-60)))}
       (os/scale {:y 8/10}
                 (double-rounded-cylinder {:radius          outer-r
                                           :height          (/ outer-r 4)
                                           :rounding-radius (tube-clamping-part-rounding-radius outer-r)}))))
     ;; tube cutout
     (chamfered-cylinder inner-r length)
     ;; cutting away one half
     (os/translate {:x (- outer-r)} (os/cube [(* 2 outer-r) outer-r length]))
     (os/rotate {:x 60}
                ;; tube seat cutout
                (os/translate {:z centerline-offset}
                              (os/cylinder {:radius inner-r
                                            :height tube-diameter}))
                ;; bolt head clearance
                (os/cylinder {:radius (* 2 bolt-radius)
                              :height (- length inner-r 2)})
                ;; bolt through-hole
                (os/cylinder {:radius bolt-radius
                              :height length}))
     ;; holes for press inserts
     (os/translate {:x clamping-offset :z (/ length 2)}
                   (os/rotate {:x 90} (insert-hole insert-hole-radius)))
     (os/translate {:x (- clamping-offset) :z (/ length 2)}
                   (os/rotate {:x 90} (insert-hole insert-hole-radius))))))

(comment
  (os/write
   (corner-end {:tube-diameter 20.3
                :bolt          :m5
                :press-fit     :m4})))

(defn- clamp-squish [inner-r outer-r]
  (/ (+ inner-r (/ outer-r) 2) outer-r))

(defn corner-clamp
  "Returns OpenSCAD string object for delta printer corner clamp. 
  Takes tube-diameter, length, and bolt params."
  [{:keys [tube-diameter length bolt]
    :or {length      (corner-end-length tube-diameter)
         align-hole? false}}]
  (let [inner-r         (/ tube-diameter 2)
        outer-r         (tube-clamping-part-outer-r inner-r)
        clamping-offset (tube-clamping-part-bolt-offset inner-r outer-r)
        bolt-r          (get-in bolts-spec [bolt :hole-radius])
        cutout          (/ tube-diameter 10)]
    (os/difference
     (os/scale {:y (clamp-squish (- inner-r cutout) outer-r)}
               (double-rounded-cylinder {:radius          outer-r
                                         :height          length
                                         :rounding-radius (tube-clamping-part-rounding-radius outer-r)}))
     ;; tube cutout
     (os/translate {:y cutout}
                   (chamfered-cylinder inner-r length))
     ;; cutting away one half, with offset to provide clamping clearence
     (os/translate {:x (- outer-r)}
                   (os/cube [(* 2 outer-r) outer-r length]))
     ;; mounting bolt holes
     (os/translate {:x clamping-offset :z (/ length 2)}
                   (os/rotate {:x 90} (os/cylinder {:radius bolt-r :height outer-r})))
     (os/translate {:x (- clamping-offset) :z (/ length 2)}
                   (os/rotate {:x 90} (os/cylinder {:radius bolt-r :height outer-r})))
     ;; mount bolt hole clearences
     (os/translate {:x clamping-offset :y (- (/ outer-r 4)) :z (/ length 2)}
                   (os/rotate {:x 90} (os/cylinder {:radius (* 2 bolt-r) :height outer-r})))
     (os/translate {:x (- clamping-offset) :y (- (/ outer-r 4)) :z (/ length 2)}
                   (os/rotate {:x 90} (os/cylinder {:radius (* 2 bolt-r) :height outer-r}))))))

(comment
  ;; Clamp for corner ends
  (os/write
   (corner-clamp {:tube-diameter 20.3
                  :bolt          :m4})))

(defn- idler-channel []
  (let [squish 0.5]
    (os/difference
     (os/hull
      (os/cylinder {:radius idler-channel-width :height idler-channel-width})
      (os/translate {:x (- idler-channel-width)}
                    (os/cylinder {:radius idler-channel-width :height idler-channel-width})))
     (os/translate {:x (- idler-channel-width)}
                   (os/cylinder {:radius        idler-channel-width
                                 :second-radius 3
                                 :height        squish}))
     (os/translate {:x (- idler-channel-width) :z (- idler-channel-width squish)}
                   (os/cylinder {:radius        3
                                 :second-radius idler-channel-width
                                 :height        squish})))))

(defn- beam-clamping-offset [beam-side] (+ (/ beam-side 2) 7))

(defn corner
  "Returns OpenSCAD string object for delta printer corner. 
  Takes tube-diameter, press-fit, beam-side and optional top? params."
  [{:keys [tube-diameter press-fit beam-side top?]
    :or {top? false}}]
  (let [width                (+ beam-side 26)
        inner-r              (/ tube-diameter 2)
        half-height          (tube-clamping-part-outer-r inner-r)
        tube-clamping-offset (tube-clamping-part-bolt-offset inner-r half-height)
        beam-clamping-offset (beam-clamping-offset beam-side)
        thickness            (+ inner-r idler-channel-width belt-beam-offset)
        belt-cutout-offset   (- (+ belt-beam-offset idler-channel-width) 2)
        rounding-radius      (tube-clamping-part-rounding-radius half-height)
        insert-hole-radius   (get-in bolts-spec [press-fit :insert-hole-radius])]
    (println (format "Beam offset: %s" (+ thickness beam-side)))
    (println (format "Idler wheel pin length: %s" (+ belt-cutout-offset 3)))
    (os/difference
     ;; Main body
     (os/union
      (when top?
        (os/translate {:y (- (- thickness 6))}
                      (os/hull
                       (os/translate {:x (- half-height rounding-radius)
                                      :z (- width rounding-radius)}
                                     (os/rotate {:x 90} (os/cylinder {:radius rounding-radius
                                                                      :height 6})))
                       (os/translate {:x (- (- half-height rounding-radius))
                                      :z (- width rounding-radius)}
                                     (os/rotate {:x 90} (os/cylinder {:radius rounding-radius
                                                                      :height 6})))
                       (os/translate {:x (- half-height rounding-radius)
                                      :z (+ 10 (- width rounding-radius))}
                                     (os/rotate {:x 90} (os/cylinder {:radius rounding-radius
                                                                      :height 6}))))))
      (os/hull
       (os/translate {:x (- half-height rounding-radius)
                      :z rounding-radius}
                     (os/rotate {:x 90} (os/cylinder {:radius rounding-radius
                                                      :height thickness})))
       (os/translate {:x (- (- half-height rounding-radius))
                      :z rounding-radius}
                     (os/rotate {:x 90} (os/cylinder {:radius rounding-radius
                                                      :height thickness})))
       (os/translate {:x (- half-height rounding-radius)
                      :z (- width rounding-radius)}
                     (os/rotate {:x 90} (os/cylinder {:radius rounding-radius
                                                      :height thickness})))
       (os/translate {:x (- (- half-height rounding-radius))
                      :z (- width rounding-radius)}
                     (os/rotate {:x 90} (os/cylinder {:radius rounding-radius
                                                      :height thickness})))))
     ;; Tube cutout
     (chamfered-cylinder inner-r width)
     ;; Holes for tube clamping press fit inserts
     (os/translate {:x tube-clamping-offset
                    :z (/ width 2)}
                   (os/rotate {:x 90} (insert-hole insert-hole-radius)))
     (os/translate {:x (- tube-clamping-offset)
                    :z (/ width 2)}
                   (os/rotate {:x 90} (insert-hole insert-hole-radius)))
     ;; Holes for beam clamping through bolts
     (os/translate {:z (+ (/ width 2) beam-clamping-offset)}
                   (os/rotate {:x 90} (os/cylinder {:radius 2.7 :height thickness})))
     (os/translate {:z (- (/ width 2) beam-clamping-offset)}
                   (os/rotate {:x 90} (os/cylinder {:radius 2.7 :height thickness})))
     ;; Clearence for beam clamping through bolts
     (os/translate {:z (+ (/ width 2) beam-clamping-offset)}
                   (os/rotate {:x 90} (os/cylinder {:radius 5 :height (- thickness 10)
                                                    :resolution 6})))
     (os/translate {:z (- (/ width 2) beam-clamping-offset)}
                   (os/rotate {:x 90} (os/cylinder {:radius 5 :height (- thickness 10)
                                                    :resolution 6})))
     ;; idler housing & axis
     (when top?
       (os/translate {:x half-height
                      :y (- (- thickness belt-cutout-offset))
                      :z (/ width 2)}
                     (os/rotate {:x 90}
                                (idler-channel)
                                (os/translate {:x (- idler-channel-width)
                                               :z -3}
                                              (os/cylinder {:radius 2.6
                                                            :height (+ belt-cutout-offset 3)}))))))))

(comment
  (os/write
   (corner {:tube-diameter 20.3
            :press-fit :m4
            :beam-side 30.4
            :top? true})))

(defn nema-17-mount
  "Returns OpenSCAD string object nema 17 mount to flat sided rectangular beam. 
  Takes beam side, press-fit and M3 screw length as parameters"
  [{:keys [beam-side press-fit screw-length]}]
  (let [clamping-offset (beam-clamping-offset beam-side)]
    ))
