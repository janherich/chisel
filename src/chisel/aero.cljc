(ns chisel.aero
  "Wings, airplanes, aero shapes"
  (:require [chisel.protocols :as protocols]
            [chisel.curves :as curves]
            [chisel.conic-sections :as conics]
            [chisel.coordinates :as c]
            [chisel.utils :as u]
            [chisel.open-scad :as os]
            [chisel.gcode :as gcode]
            [chisel.gcode-layers :as gcode-layers]))

(defprotocol PAirfoil
  "Protocol for airfoil sections"
  (code [this]
    "Returns identifier of the airfoil section.")
  (upper-curve [this]
    "Returns parametric curve for upper (suction) side of airfoil normalized to unit chord
    with TE point fixed to `[0 0]` and LE point to `[1 0]`.")
  (lower-curve [this]
    "Returns parametric curve for lower (pressure) side of airfoil normalized to unit chord
    with TE point fixed to `[0 0]` and LE point to `[1 0]`.")
  (dat-coordinates [this resolution]
    "Returns `.dat` coordinate content string of unit chord airfoil with given resolution."))

(defprotocol PWing
  "Protocol for wings"
  (upper-wing-patch [this span section]
    "Returns upper wing patch of given span, keyword `section` deciding whether it's original half, mirror half or full wing patch")
  (lower-wing-patch [this span section]
    "Returns lower wing patch of given span, keyword `section` deciding whether it's original half, mirror half or full wing patch")
  (chord-length [this span position]
    "Returns chord length for wing of given span at position in `[0 1]` interval"))

(defn airfoil->dat
  "Converts airfoil represented by upper and lower curve (direction LE->TE) 
  into `.dat` coordinate file string."
  [upper-curve lower-curve curve-resolution]
  (let [upper-points (protocols/polyline upper-curve curve-resolution)
        lower-points (protocols/polyline lower-curve curve-resolution)]
    (apply str
           (map (fn [point]
                  (str (format "%f" (point 0)) "  " (format "%f" (point 1)) "\n"))
                (concat (reverse upper-points)
                        lower-points)))))

(defrecord Airfoil [upper-curve lower-curve code]
  PAirfoil
  (code [_] code)
  (upper-curve [_] upper-curve)
  (lower-curve [_] lower-curve)
  (dat-coordinates [_ resolution]
    (airfoil->dat upper-curve lower-curve resolution))
  protocols/PTransformable
  (linear-transform [this matrix]
    (assoc this
           :upper-curve (protocols/linear-transform upper-curve matrix)
           :lower-curve (protocols/linear-transform lower-curve matrix))))

(defn lr-airfoil
  "Creates low-reynolds type airfoil implementing `PAirfoil` protocol/interface, parametrized
  by thickness and camber percentages & ratio in `[0 1]` interval representing position of max camber."
  [{:keys [thickness camber max-camber] :or {max-camber 1/3}}]
  (let [half-thickness        (/ thickness 200)
        start-point           (c/v [0 0])
        mid-point             (c/v [max-camber (/ camber 100)])
        end-point             (c/v [1 0])
        [u-start u-mid u-end] (u/distanced-path [start-point mid-point end-point] half-thickness)
        [l-start l-mid l-end] (u/distanced-path [start-point mid-point end-point] (- half-thickness))
        curve-fn              (fn [p1 p2 p3 p4 p5]
                                (curves/clamped-b-spline
                                 {:control-points [p1
                                                   (c/weighted p2 conics/WEIGHT_90)
                                                   (c/sum p2
                                                          (c/scale-vector (c/difference p3 p2) half-thickness))
                                                   p3
                                                   (c/sum p4
                                                          (c/scale-vector (c/difference p3 p4) half-thickness))
                                                   (c/weighted p4 conics/WEIGHT_90)
                                                   p5]
                                  :knot-vector    [1/3 1/3 2/3 2/3]
                                  :order          2}))]
    (map->Airfoil {:upper-curve (curve-fn start-point u-start u-mid u-end end-point)
                   :lower-curve (curve-fn start-point l-start l-mid l-end end-point)
                   :code        "LR_Airfoil"})))

(defn cubic-airfoil
  "Creates basic cubic airfoil type implementing `PAirfoil` protocol/interface, parametrized
  by leading edge radius, trailing edge boat angle and max camber position of upper/lower surfaces"
  [{:keys [upper-le-radius upper-te-angle upper-max-camber
           lower-le-radius lower-te-angle lower-max-camber]}]
  (let [upper-te-angle-rad    (Math/toRadians upper-te-angle)
        lower-te-angle-rad    (Math/toRadians lower-te-angle)
        start-point           (c/v [0 0])
        upper-le-radius-point (c/v [0 (* 1/3 (Math/sqrt (/ upper-le-radius 50)))])
        upper-te-angle-point  (c/v [upper-max-camber (* (- 1 upper-max-camber)
                                                        (Math/tan upper-te-angle-rad))])
        lower-le-radius-point (c/v [0 (* -1/3 (Math/sqrt (/ lower-le-radius 50)))])
        lower-te-angle-point  (c/v [lower-max-camber (* -1
                                                        (- 1 lower-max-camber)
                                                        (Math/tan lower-te-angle-rad))])
        
        end-point             (c/v [1 0])]
    (map->Airfoil {:upper-curve (curves/bezier-curve [start-point upper-le-radius-point upper-te-angle-point end-point])
                   :lower-curve (curves/bezier-curve [start-point lower-le-radius-point lower-te-angle-point end-point])
                   :code        "Cubic_Airfoil"})))

(defn symmetric-cubic-airfoil
  "Creates cubic airfoil with symmetric upper/lower curves"
  [{:keys [le-radius te-angle max-camber] :or {max-camber 1/3}}]
  (let [te-angle-half (/ te-angle 2)]
    (cubic-airfoil {:upper-le-radius  le-radius
                    :upper-te-angle   te-angle-half
                    :upper-max-camber max-camber
                    :lower-le-radius  le-radius
                    :lower-te-angle   te-angle-half
                    :lower-max-camber max-camber})))

(comment
  (def symmetric-cubic (cambered-cubic-airfoil {:le-radius          0.4
                                                :te-angle           12})))

(defn cambered-cubic-airfoil
  "Creates cubic airfoil with given upper/lower te ratio"
  [{:keys [le-radius te-angle max-camber lower-camber-ratio] :or {max-camber 1/3}}]
  (let [lower-te-angle (* te-angle lower-camber-ratio)
        upper-te-angle (* te-angle (- 1 lower-camber-ratio))]
    (cubic-airfoil {:upper-le-radius  le-radius
                    :upper-te-angle   upper-te-angle
                    :upper-max-camber max-camber
                    :lower-le-radius  le-radius
                    :lower-te-angle   lower-te-angle
                    :lower-max-camber max-camber})))

#_(comment)
(def cubic (cambered-cubic-airfoil {:le-radius          0.42
                                    :te-angle           10.7
                                    :max-camber         9/40
                                    :lower-camber-ratio 2/9}))

(defn reflexed-cubic-airfoil
  "Creates basic cubic airfoil with reflexed trailing edge implementing `PAirfoil` protocol/interface."
  [{:keys [le-radius
           upper-te-angle upper-max-camber upper-reflex
           lower-te-angle lower-max-camber lower-reflex]}]
  (let [upper-te-angle-rad    (Math/toRadians upper-te-angle)
        lower-te-angle-rad    (Math/toRadians lower-te-angle)
        start-point           (c/v [0 0])
        upper-le-radius-point (c/v [0 (* 1/3 (Math/sqrt (/ le-radius 50)))])
        upper-te-angle-point  (c/v [upper-max-camber (* (- 1 upper-max-camber) (Math/tan upper-te-angle-rad))])
        lower-le-radius-point (c/v [0 (* -1/3 (Math/sqrt (/ le-radius 50)))])
        lower-te-angle-point  (c/v [lower-max-camber (* -1 (- 1 lower-max-camber) (Math/tan lower-te-angle-rad))])
        end-point             (c/v [1 0])
        upper-te-angle-end-v  (c/difference end-point upper-te-angle-point)
        upper-reflex-point    (c/sum (c/linear-combination upper-max-camber upper-te-angle-point end-point)
                                     (c/scale-vector (c/orthogonal-vector upper-te-angle-end-v)
                                                     (* (c/vector-length upper-te-angle-end-v)
                                                        (/ upper-reflex 100))))
        lower-te-angle-end-v  (c/difference end-point lower-te-angle-point)
        lower-reflex-point    (c/sum (c/linear-combination lower-max-camber lower-te-angle-point end-point)
                                     (c/scale-vector (c/orthogonal-vector lower-te-angle-end-v
                                                                          :counterclockwise? true)
                                                     (* (c/vector-length lower-te-angle-end-v)
                                                        (/ lower-reflex 100))))]
    (map->Airfoil {:upper-curve (curves/clamped-uniform-b-spline
                                 {:control-points [start-point upper-le-radius-point upper-te-angle-point upper-reflex-point end-point]
                                  :order          3})
                   :lower-curve (curves/clamped-uniform-b-spline
                                 {:control-points [start-point lower-le-radius-point lower-te-angle-point lower-reflex-point end-point]
                                  :order          3})
                   :code        "Reflexed_Cubic_Airfoil"})))

(comment
  (def reflexed (reflexed-cubic-airfoil {:le-radius        0.4
                                         :upper-te-angle   7
                                         :lower-te-angle   3
                                         :upper-reflex     1
                                         :lower-reflex     -2
                                         :upper-max-camber 7/20
                                         :lower-max-camber 4/5})))

(defn reflexed-quartic-airfoil
  "Reflexed quartic airfoil"
  [{:keys [le-up s-bend-up s-bend-position
           le-down downtail downtail-position]}]
  (let [start-point    (c/v [0 0])
        upper-le-point (c/v [0 (/ le-up 10)])
        s-bend-point-1 (c/v [s-bend-position (/ s-bend-up 100)])
        s-bend-point-2 (c/v [s-bend-position 0])
        end-point      (c/v [1 0])
        lower-le-point (c/v [0 (- (/ le-down 10))])
        downtail-point (c/v [downtail-position (- (/ downtail 100))])]
    (map->Airfoil {:upper-curve (curves/bezier-curve [start-point upper-le-point
                                                      s-bend-point-1 s-bend-point-2 end-point])
                   :lower-curve (curves/bezier-curve [start-point lower-le-point
                                                      downtail-point end-point])
                   :code        "Reflexed_Quartic_Airfoil"})))

(comment
  (def reflexed (reflexed-quartic-airfoil {:le-up             0.6
                                           :s-bend-up         12
                                           :s-bend-position   6/10
                                           :le-down           0.5
                                           :downtail          1
                                           :downtail-position 3/4})))

(defn test-airfoil [airfoil export?]
  (if export?
    (u/write-to-file (str "/Users/janherich/CAD/" (code airfoil) ".dat")
                     (dat-coordinates airfoil 100))
    (let [scaled (protocols/linear-transform airfoil (c/scale-matrix 100))]
      (os/write
       (os/generate-polygon
        (into (protocols/polyline (upper-curve scaled) 200)
              (rseq (protocols/polyline (lower-curve scaled) 200))))))))

(defn- transform-airfoil
  "Transforms airfoil with unit chord parallel to X axis according to given LE/TE points"
  [airfoil le-point te-point]
  (let [chord-vector  (c/difference te-point le-point)
        chord-length  (c/vector-length chord-vector)
        scaling       (c/scale-matrix (Math/abs chord-length))
        rot-direction (if (pos? (chord-vector 1)) -1.0 1.0)
        rotation      (c/rotate-matrix (if (zero? chord-length)
                                         0
                                         (* rot-direction (Math/acos (/ (chord-vector 0) chord-length)))))
        translation  (c/translate-matrix le-point)]
    (protocols/linear-transform airfoil (c/combine-matrices translation scaling rotation))))

(defn- prepare-control-edge [edge span section]
  (let [original-edge (protocols/linear-transform edge (c/scale-matrix span))]
    (case section
      :mirror (protocols/linear-transform original-edge (c/flip-matrix :y))
      :right  (protocols/linear-transform original-edge (c/flip-matrix :z))
      original-edge)))

(defn- wing-patch [airfoil-side leading-edge-curve trailing-edge-curve span section]
  (let [airfoil-curve (cond-> airfoil-side
                        (= section :mirror)
                        (protocols/linear-transform (c/flip-matrix :y)))
        le   (prepare-control-edge leading-edge-curve span section)
        te   (prepare-control-edge trailing-edge-curve span section)]
    (curves/tensor-product-patch (fn [[le-point te-point]]
                                   (transform-airfoil airfoil-curve le-point te-point))
                                 [le te])))

(defrecord ConstantAirfoilWing [leading-edge-curve trailing-edge-curve airfoil]
  PWing
  (upper-wing-patch [_ span section]
    (wing-patch (upper-curve airfoil) leading-edge-curve trailing-edge-curve span section))
  (lower-wing-patch [_ span section]
    (wing-patch (lower-curve airfoil) leading-edge-curve trailing-edge-curve span section))
  (chord-length [_ span position]
    (u/relative-parameter-assertion position)
    (Math/abs (c/vector-length
               (c/difference
                (protocols/curve-point (prepare-control-edge trailing-edge-curve span nil) position)
                (protocols/curve-point (prepare-control-edge leading-edge-curve span nil) position)))))
  protocols/PTransformable
  (linear-transform [this matrix]
    (assoc this
           :leading-edge-curve  (protocols/linear-transform leading-edge-curve matrix)
           :trailing-edge-curve (protocols/linear-transform trailing-edge-curve matrix))))

(defn straight-wing
  "Wing with constant chord and airfoil section"
  [{:keys [airfoil aspect-ratio sweep twist] :or {sweep 0 twist 0}}]
  (let [half-span     (/ 1 aspect-ratio)
        tip-translate (Math/tan (Math/toRadians sweep))]
    (map->ConstantAirfoilWing
     {:leading-edge-curve  (curves/bezier-curve [(c/v [(- half-span) 0 0])
                                                 (protocols/linear-transform
                                                  (c/v [(- half-span) 0 1])
                                                  (c/combine-matrices
                                                   (c/translate-matrix [tip-translate 0 0])
                                                   (c/rotate-matrix (Math/toRadians twist))))])
      :trailing-edge-curve (curves/bezier-curve [(c/v [half-span 0 0])
                                                 (protocols/linear-transform
                                                  (c/v [half-span 0 1])
                                                  (c/combine-matrices
                                                   (c/translate-matrix [tip-translate 0 0])
                                                   (c/rotate-matrix (Math/toRadians twist))))])
      :airfoil             airfoil})))

(defn tapered-wing
  "Tapered wing"
  [{:keys [airfoil taper aspect-ratio sweep twist] :or {sweep 0 twist 0}}]
  (let [half-span     (/ 1 aspect-ratio)
        tip-translate (Math/tan (Math/toRadians sweep))]
    (map->ConstantAirfoilWing
     {:leading-edge-curve  (curves/bezier-curve [(c/v [(- half-span) 0 0])
                                                 (protocols/linear-transform
                                                  (c/v [(- half-span) 0 1])
                                                  (c/combine-matrices
                                                   (c/translate-matrix [tip-translate 0 0])
                                                   (c/rotate-matrix (Math/toRadians twist))
                                                   (c/scale-matrix {:x taper
                                                                    :y taper})))])
      :trailing-edge-curve (curves/bezier-curve [(c/v [half-span 0 0])
                                                 (protocols/linear-transform
                                                  (c/v [half-span 0 1])
                                                  (c/combine-matrices
                                                   (c/translate-matrix [tip-translate 0 0])
                                                   (c/rotate-matrix (Math/toRadians twist))
                                                   (c/scale-matrix {:x taper
                                                                    :y taper})))])
      :airfoil             airfoil})))

(defn- normalize-wing-curves [wing resolution]
  (-> wing
      (update :leading-edge-curve #(curves/axis-uniform-curve (protocols/polyline % resolution) :z))
      (update :trailing-edge-curve #(curves/axis-uniform-curve (protocols/polyline % resolution) :z))))

(defn elliptic-wing
  "Elliptic wing"
  [{:keys [airfoil taper aspect-ratio twist] :or {taper 0 twist 0}}]
  (let [half-span     (/ 1 aspect-ratio)]
    (-> (map->ConstantAirfoilWing
         {:leading-edge-curve (curves/clamped-uniform-b-spline
                               {:control-points [(c/v [(* 11/10 (- half-span)) 0 0])
                                                 (c/v [(- half-span) 0 0])
                                                 (c/v [(- half-span) 0 1/10])
                                                 (protocols/linear-transform
                                                  (c/weighted (c/v [(- half-span) 0 1])
                                                              conics/WEIGHT_90)
                                                  (c/rotate-matrix (Math/toRadians twist)))
                                                 (protocols/linear-transform
                                                  (c/v [(- half-span) 0 1])
                                                  (c/combine-matrices
                                                   (c/rotate-matrix (Math/toRadians twist))
                                                   (c/scale-matrix {:x taper
                                                                    :y taper})))]
                                :order          2})
          :trailing-edge-curve (curves/clamped-uniform-b-spline
                                {:control-points [(c/v [(* 11/10 half-span) 0 0])
                                                  (c/v [half-span 0 0])
                                                  (c/v [half-span 0 1/10])
                                                  (protocols/linear-transform
                                                   (c/weighted (c/v [half-span 0 1])
                                                               conics/WEIGHT_90)
                                                   (c/rotate-matrix (Math/toRadians twist)))
                                                  (protocols/linear-transform
                                                   (c/v [half-span 0 1])
                                                   (c/combine-matrices
                                                    (c/rotate-matrix (Math/toRadians twist))
                                                    (c/scale-matrix {:x taper
                                                                     :y taper})))]
                                 :order          2})
          :airfoil             airfoil})
        (normalize-wing-curves 1000))))

(defn flying-wing
  "Flying wing"
  [{:keys [airfoil taper aspect-ratio twist sweep body-ratio] :or {taper 1/2 body-ratio 2 sweep 20}}]
  (let [half-span     (/ 1 aspect-ratio)
        tip-translate (Math/tan (Math/toRadians sweep))]
    (-> (map->ConstantAirfoilWing
         {:leading-edge-curve  (curves/bezier-curve [(c/v [(- half-span) 0 0])
                                                     (protocols/linear-transform
                                                      (c/v [(- half-span) 0 1])
                                                      (c/combine-matrices
                                                       (c/translate-matrix [tip-translate 0 0])
                                                       (c/rotate-matrix (Math/toRadians twist))
                                                       (c/scale-matrix {:x taper
                                                                        :y taper})))])
          :trailing-edge-curve (curves/bezier-curve [(c/v [(* body-ratio half-span) 0 0])
                                                     (c/v [half-span 0 0])
                                                     (protocols/linear-transform
                                                      (c/v [half-span 0 1])
                                                      (c/combine-matrices
                                                       (c/translate-matrix [tip-translate 0 0])
                                                       (c/rotate-matrix (Math/toRadians twist))
                                                       (c/scale-matrix {:x taper
                                                                        :y taper})))])
          :airfoil             airfoil})
        (normalize-wing-curves 500))))

(defn- shift-z [patch z]
  (protocols/linear-transform patch (c/translate-matrix [0 0 z])))

(defn fuselage-mesh []
  #_(let ))

(defn render-wing [wing]
  (os/write
   (os/generate-polyhedron
    (u/merge-triangle-meshes
     (protocols/triangle-mesh (shift-z (upper-wing-patch wing 100 :left) 5) [1000 100])
     (protocols/triangle-mesh (shift-z (lower-wing-patch wing 100 :left) 5) [1000 100])
     (protocols/triangle-mesh (shift-z (upper-wing-patch wing 100 :right) -5) [1000 100])
     (protocols/triangle-mesh (shift-z (lower-wing-patch wing 100 :right) -5) [1000 100])))))

(def ^:private layers-per-mm 5)

(defn wing-gcode [wing span mirror?]
  (let [top-patch       (upper-wing-patch wing span (when mirror? :mirror :left))
        bottom-patch    (lower-wing-patch wing span (when mirror? :mirror :left))]
    (merge {:skirt-polyline (gcode/circular-polyline (+ 5 (/ (chord-length wing span 0) 2)))}
           (gcode-layers/closed-ribbed-panel-descriptor top-patch bottom-patch
                                                        (int (/ span 20))
                                                        (* layers-per-mm span)
                                                        200)
           #_(gcode-layers/corrugated-panel-descriptor top-patch bottom-patch
                                                     #_1 5
                                                     (* layers-per-mm span)
                                                     200
                                                     :skin-line-width 0.4
                                                     :corrugate-fn
                                                     #_(constantly [[true 1/3] [false 1/3]]) (partial gcode-layers/cutoff-corrugations 1/15)
                                                     :modulate-curve (gcode-layers/sine-curve 1/30 10)))))

(def uav-airfoil (cambered-cubic-airfoil {:le-radius          0.42
                                          :te-angle           10.7
                                          :max-camber         9/40
                                          :lower-camber-ratio 2/9}))

(def uav-wing (elliptic-wing {:airfoil      uav-airfoil
                              :aspect-ratio 10
                              :taper        1/10}))

(defn wing-patches [wing span cut-range section]
  [(curves/cut-patch (upper-wing-patch wing span section) cut-range)
   (curves/cut-patch (lower-wing-patch wing span section) cut-range)])

(defn wing-1 [section]
  (let [[top bottom] (wing-patches uav-wing 700 [0 (/ 250 700)] section)]
    (merge {:skirt-polyline (gcode/circular-polyline (+ 5 (/ (chord-length uav-wing 700 0) 2)))}
           (gcode-layers/corrugated-panel-descriptor top bottom
                                                     6
                                                     (* layers-per-mm 250)
                                                     200
                                                     :skin-line-width 0.4
                                                     :corrugate-fn
                                                     (partial gcode-layers/cutoff-corrugations 1/15)))))

(comment
  (u/write-to-file
   "/Users/janherich/CAD/uav_left_wing_1.gcode"
   (gcode/generate-gcode (merge gcode/qqs-print-descriptor
                                gcode/lw-pla-print-descriptor
                                (wing-1 :left)))))

(defn wing-2 [section]
  (let [[top bottom] (wing-patches uav-wing 700 [(/ 250 700) (/ 500 700)] section)]
    (merge {:skirt-polyline (gcode/circular-polyline (+ 5 (/ (chord-length uav-wing 700 (/ 250 700)) 2)))}
           (gcode-layers/corrugated-panel-descriptor (protocols/linear-transform
                                                      top (c/translate-matrix [0 0 (- 250)]))
                                                     (protocols/linear-transform
                                                      bottom (c/translate-matrix [0 0 (- 250)]))
                                                     6
                                                     (* layers-per-mm 250)
                                                     200
                                                     :skin-line-width 0.4
                                                     :corrugate-fn
                                                     (partial gcode-layers/cutoff-corrugations 1/15)))))

(comment
  (u/write-to-file
   "/Users/janherich/CAD/uav_left_wing_2.gcode"
   (gcode/generate-gcode (merge gcode/qqs-print-descriptor
                                gcode/lw-pla-print-descriptor
                                (wing-2 :left)))))

(defn wing-3 [section]
  (let [[top bottom] (wing-patches uav-wing 700 [(/ 500 700) (/ 700 700)] section)]
    (merge {:skirt-polyline (gcode/circular-polyline (+ 5 (/ (chord-length uav-wing 700 (/ 500 700)) 2)))}
           (gcode-layers/corrugated-panel-descriptor (protocols/linear-transform
                                                      top (c/translate-matrix [0 0 (- 500)]))
                                                     (protocols/linear-transform
                                                      bottom (c/translate-matrix [0 0 (- 500)]))
                                                     6
                                                     (* layers-per-mm 200)
                                                     200
                                                     :skin-line-width 0.4
                                                     :corrugate-fn
                                                     (partial gcode-layers/cutoff-corrugations 1/15)))))

(comment
  (u/write-to-file
   "/Users/janherich/CAD/uav_left_wing_3.gcode"
   (gcode/generate-gcode (merge gcode/qqs-print-descriptor
                                gcode/lw-pla-print-descriptor
                                (wing-3 :left)))))


(comment
  (u/write-to-file
   "/Users/janherich/CAD/wing.gcode"
   (gcode/generate-gcode (merge gcode/qqs-print-descriptor
                                gcode/lw-pla-print-descriptor
                                (wing-gcode
                                 (elliptic-wing {:airfoil      uav-airfoil
                                                 :aspect-ratio 5
                                                 :taper        1/3
                                                 :twist        -2})
                                 200
                                 false)))))

