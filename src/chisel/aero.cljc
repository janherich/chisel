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
  [{:keys [upper-le-radius upper-te-angle upper-max-camber lower-le-radius lower-te-angle lower-max-camber]}]
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

(defn test-airfoil [airfoil export?]
  (if export?
    (u/write-to-file (str "/Users/janherich/CAD/" (code airfoil) ".dat")
                     (dat-coordinates airfoil 100))
    (let [scaled (protocols/linear-transform airfoil (c/scale-matrix 100))]
      (os/write
       (os/generate-polygon
        (into (protocols/polyline (upper-curve scaled) 100)
              (rseq (protocols/polyline (lower-curve scaled) 100))))))))

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

(defn test-transform [le te]
  (let [scaled (transform-airfoil (symmetric-cubic-airfoil {:le-radius 1.5 :te-angle 10}) le te)]
    (os/write
     (os/generate-polygon
      (into (protocols/polyline (upper-curve scaled) 100)
            (rseq (protocols/polyline (lower-curve scaled) 100)))))))

(defn- prepare-control-edge [edge span section]
  (let [original-edge (protocols/linear-transform edge (c/scale-matrix span))]
    (case section
      :mirror (protocols/linear-transform original-edge (c/flip-matrix :y))
      :full   (curves/join-curves (protocols/linear-transform (curves/invert-curve original-edge)
                                                              (c/flip-matrix :z))
                                  original-edge)
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

(defn elliptic-wing
  "Elliptic wing"
  [{:keys [airfoil taper aspect-ratio twist] :or {taper 0 twist 0}}]
  (let [half-span     (/ 1 aspect-ratio)]
    (map->ConstantAirfoilWing
     {:leading-edge-curve  (curves/bezier-curve [(c/v [(- half-span) 0 0])
                                                 (protocols/linear-transform
                                                  (c/weighted (c/v [(- half-span) 0 1])
                                                              conics/WEIGHT_90)
                                                  (c/rotate-matrix (Math/toRadians twist)))
                                                 (protocols/linear-transform
                                                  (c/v [(- half-span) 0 1])
                                                  (c/combine-matrices
                                                   (c/rotate-matrix (Math/toRadians twist))
                                                   (c/scale-matrix {:x taper
                                                                    :y taper})))])
      :trailing-edge-curve (curves/bezier-curve [(c/v [half-span 0 0])
                                                 (protocols/linear-transform
                                                  (c/weighted (c/v [half-span 0 1])
                                                              conics/WEIGHT_90)
                                                  (c/rotate-matrix (Math/toRadians twist)))
                                                 (protocols/linear-transform
                                                  (c/v [half-span 0 1])
                                                  (c/combine-matrices
                                                   (c/rotate-matrix (Math/toRadians twist))
                                                   (c/scale-matrix {:x taper
                                                                    :y taper})))])
      :airfoil             airfoil})))

#_(defn flying-wing
  "Flying wing"
  [{:keys [airfoil taper aspect-ratio sweep body-ratio] :or {taper 1/2 body-ratio 1/10 sweep 20}}]
  (let [half-span     (/ 1 aspect-ratio)
        
        tip-translate (Math/tan (Math/toRadians sweep))]
    (map->ConstantAirfoilWing
     {:leading-edge-curve  (curves/bezier-curve [(c/v [(- half-span) 0 0])
                                                 (protocols/linear-transform
                                                  (c/v [(- half-span) 0 1])
                                                  (c/combine-matrices
                                                   (c/translate-matrix [tip-translate 0 0])
                                                   (c/scale-matrix {:x taper
                                                                    :y taper})))])
      :trailing-edge-curve (curves/bezier-curve [(c/v [half-span 0 0])
                                                 (protocols/linear-transform
                                                  (c/v [half-span 0 1])
                                                  (c/combine-matrices
                                                   (c/translate-matrix [tip-translate 0 0])
                                                   (c/scale-matrix {:x taper
                                                                    :y taper})))])
      :airfoil             airfoil})))

(defn render-wing [wing]
  (os/write
   (os/generate-polyhedron
    (u/merge-triangle-meshes
     (protocols/triangle-mesh (upper-wing-patch wing 500 :full) [200 100])
     (protocols/triangle-mesh (lower-wing-patch wing 500 :full) [200 100])))))

(def ^:private layers-per-mm 5)

(defn- normalize-wing-curves [wing span]
  (-> wing
      (update :leading-edge-curve #(curves/axis-uniform-curve (protocols/polyline % span) :z))
      (update :trailing-edge-curve #(curves/axis-uniform-curve (protocols/polyline % span) :z))))

(defn wing-gcode [wing span mirror?]
  (let [normalized-wing (normalize-wing-curves wing span)
        top-patch       (upper-wing-patch normalized-wing span (when mirror? :mirror))
        bottom-patch    (lower-wing-patch normalized-wing span (when mirror? :mirror))]
    (merge {:skirt-polyline (gcode/circular-polyline (+ 5 (/ (chord-length normalized-wing span 0) 2)))}
           (gcode-layers/corrugated-panel-descriptor top-patch bottom-patch
                                                     5
                                                     (* layers-per-mm span)
                                                     200
                                                     :skin-line-width 0.4
                                                     :corrugate-fn
                                                     (partial gcode-layers/cutoff-corrugations 1/15)))))

(comment
  (u/write-to-file
   "/Users/janherich/CAD/wing.gcode"
   (gcode/generate-gcode (merge gcode/qqs-lw-pla-print-descriptor
                                (wing-gcode
                                 (elliptic-wing {:airfoil      (lr-airfoil {:thickness 2 :camber 6})
                                                 :aspect-ratio 5
                                                 :taper        1/3
                                                 :twist        -2})
                                 150
                                 false)))))

