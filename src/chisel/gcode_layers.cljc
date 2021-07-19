(ns chisel.gcode-layers
  "Gcode layer implementations"
  (:require [chisel.protocols :as protocols]
            [chisel.curves :as curves]
            [chisel.coordinates :as c]
            [chisel.utils :as u]))

(defn- alternate-direction [segment layer-idx]
  (cond-> segment
    (odd? layer-idx) (update :polyline reverse)))

(defn- decreasing-line-width [layers-count start target]
  (let [step (/ (- target start) layers-count)]
    (fn [segment layer-idx]
      (assoc segment :line-width (+ start (* step layer-idx))))))

(defn- corrugations-assertion [corrugations]
  (assert (>= corrugations 1) "Minimum of 1 corrugation must be specified"))

(comment
  (def corrugations
    [[0 0]
     [1 1/10]
     [0 2/10]
     [1 3/10]
     [0 4/10]
     [1 5/10]
     [0 6/10]
     [1 7/10]
     [0 8/10]
     [1 9/10]
     [0 10/10]]))

(def ^:private flip-side {0 1 1 0})

(defn- intersecting? [intersection-point a b]
  (or (and (< a intersection-point) (> b intersection-point))
      (and (< b intersection-point) (> a intersection-point))))

(defn- calculate-intersections [[start-value start-point] [end-value end-point]]
  (let [interval-length         (u/abs (- start-point end-point))
        zero-point-intersection (and (intersecting? 0 start-point end-point)
                                     [(u/abs (- (/ (u/abs start-point) interval-length)
                                                end-value))
                                      0])
        one-point-intersection  (and (intersecting? 1 start-point end-point)
                                     [(u/abs (- (/ (u/abs (- start-point 1)) interval-length)
                                                end-value))
                                      1])]
    (cond-> []
      zero-point-intersection (conj zero-point-intersection)
      one-point-intersection (conj one-point-intersection))))

(defn- advance-position [[value point] step]
  [(flip-side value) (+ point step)])

(defn- generate-corrugations
  [start step]
  (loop [current-position   [1 start]
         corrugations       []]
    (let [new-position     (advance-position current-position step)
          intersections    (calculate-intersections current-position new-position)
          new-corrugations (into corrugations intersections)
          leaving?         (not (u/relative-parameter-check (second new-position)))]
      (if leaving?
        new-corrugations
        (recur new-position (conj new-corrugations new-position))))))

(defn uniform-corrugations
  "Generate uniform corrugations, centered in the middle of the interval by default, can be shifted
  relatively to one corrugation width (step) by optional `:shift` kw argument."
  [corrugations & {:keys [shift] :or {shift 0}}]
  (let [step      (/ 1 corrugations 2)
        mid-point (+ 1/2 (* shift step))]
    (concat (rseq (generate-corrugations mid-point (- step)))
            [[1 mid-point]]
            (generate-corrugations mid-point step))))

(defn sine-curve
  "Returns sine curve with given frequency and amplitude.
  Optional `:shift` kw argument determines shift of the sine curve relative to one 1/f"
  [amplitude frequency & {:keys [shift] :or {shift 0}}]
  (reify protocols/PParametricCurve
    (protocols/curve-point [_ t]
      (u/relative-parameter-assertion t)
      (let [offset    (* shift (/ (* 2 Math/PI) frequency))
            angle-rad (+ offset (* t frequency (* 2 Math/PI)))]
        (* amplitude (Math/sin angle-rad))))
    (protocols/polyline [this points-count]
      (curves/resolve-curve points-count #(protocols/curve-point this %)))
    (protocols/closed? [_] true)))

(defn sine-corrugations
  "Generate uniform sine-shaped (smooth) corrugations of given resolution, centered in the middle 
  of the interval by default, can be shifted relatively to one corrugation width by optional 
  `:shift` kw argument."
  [corrugations & {:keys [shift resolution]
                   :or {shift      0
                        resolution (int (* corrugations 20))}}]
  (let [infill-curve (sine-curve 1/2 corrugations :shift (* 2 shift))]
    (map (fn [point]
           (let [curve-param (/ point (dec resolution))]
             [(+ 1/2 (protocols/curve-point infill-curve curve-param)) curve-param]))
         (range 0 resolution))))

(defn cutoff-corrugations
  "Returns cutoff corrugation distribution sequence which is uniform on `[start stop]` interval"
  ([start corrugations]
   (cutoff-corrugations start (- 1 start) corrugations))
  ([start end corrugations]
   (u/relative-parameter-assertion start)
   (u/relative-parameter-assertion end)
   (corrugations-assertion corrugations)
   (assert (> end start)
           "End parameter must be higher then start")
   (let [distance (- end start)
         step     (/ distance corrugations 2)]
     (map vector
          (iterate flip-side 0)
          ;; cumbersome take/iterate combinations necessary because range doesn't work well with floating point numbers
          (-> (take (inc (* 2 corrugations))
                    (iterate (partial + step) start))
              (conj start)
              vec
              (conj end))))))

(defn- resolve-corrugation-tuple [top-curve bottom-curve [curve-distance-parameter curve-parameter]]
  (case curve-distance-parameter
    1 (protocols/curve-point top-curve curve-parameter)
    0 (protocols/curve-point bottom-curve curve-parameter)
    (c/linear-combination curve-distance-parameter
                          (protocols/curve-point top-curve curve-parameter)
                          (protocols/curve-point bottom-curve curve-parameter))))

(defn horizontal-corrugated-infill
  "Given two facesheet polylines and connections-distributing sequence,
  generates horizontal corrugated infill between them, 
  optionally takes connecting distance argument (defaults to `0.1`)."
  [top-polyline bottom-polyline connections-distribution
   & {:keys [connecting-distance] :or {connecting-distance 0.1}}]
  (let [top-uniform-curve    (curves/uniform-curve
                              (cond-> top-polyline
                                (not (zero? connecting-distance))
                                (u/distanced-path connecting-distance)))
        bottom-uniform-curve (curves/uniform-curve
                              (cond-> bottom-polyline
                                (not (zero? connecting-distance))
                                (u/distanced-path (- connecting-distance))))]
    (mapv (partial resolve-corrugation-tuple top-uniform-curve bottom-uniform-curve)
          connections-distribution)))

(defn- layer-interval-sequence [layers]
  (let [layer-step (/ 1 layers)]
    (range 0 (+ 1 layer-step) layer-step)))

(defn- polyline->layer-slice [[first-point & polyline-rest]]
  (let [z (u/round (first-point 2))]
    {:z        z
     :polyline (into [[(first-point 0) (first-point 1)]]
                     (map (fn [point]
                            (let [point-z (u/round (point 2))]
                              (assert (= z point-z)
                                      (str "Z height must be constant in one slice, " z " /= " point-z)))
                            [(point 0) (point 1)]))
                     polyline-rest)}))

(defn- modulate-connections-distribution
  "Given connection distribution sequence parametric modulate curve and paramer t,
  modulates (shifts) connection distribution sequence according to modulate curve/t
  evaluation.
  Modulate curve should be parametric 1d curve returning point in `[0 1]` range.
  Takes care of never straying beyond allowed `[0 1]` interval for any connection."
  [connections-distribution modulate-curve t]
  (let [shift (protocols/curve-point modulate-curve t)]
    (map #(update % 1 (fn [t]
                        (if (pos? shift)
                          (min 1 (+ t shift))
                          (max 0 (+ t shift)))))
         connections-distribution)))

(defn vertical-infill-index
  "Given two facesheet patches, resolution and connections-distributing sequence,
  generates index (range-tree) returning verticall infill between them,
  optionally takes connecting distance argument (defaults to `0.10`). "
  [top-patch bottom-patch resolution connections-distribution
   & {:keys [connecting-distance] :or {connecting-distance 0.1}}]
  (u/make-range-tree
   (mapv (fn [[[first-distance t-1] [second-distance t-2]]]
           (let [first-top?   (= first-distance 1)
                 second-top?  (= second-distance 1)
                 range-diff   (- t-2 t-1)
                 first-patch  (if second-top? top-patch bottom-patch)
                 second-patch (if first-top? top-patch bottom-patch)
                 anchor-1 (u/distanced-path
                           (protocols/polyline
                            (protocols/patch-slice first-patch t-1) resolution)
                           (if first-top?
                             (- connecting-distance)
                             connecting-distance))
                 anchor-2 (u/distanced-path
                           (protocols/polyline
                            (protocols/patch-slice second-patch t-2) resolution)
                           (if first-top?
                             connecting-distance
                             (- connecting-distance)))]
             {:range          [t-1 t-2]
              :interpolate-fn (fn [t]
                                (mapv (fn [p1 p2]
                                        (c/linear-combination (/ (- t t-1) range-diff) p1 p2))
                                      anchor-1 anchor-2))}))
         (partition 2 1 connections-distribution))))

(defn calculate-skin-core-overlap
  [skin-line-width core-line-width]
  (/ (min skin-line-width core-line-width) 2))

(defn- top-patch-polyline [top-polyline bottom-polyline]
  (let [first-bottom (get bottom-polyline 0)]
    ;; polylines not connected at start
    (if (not= first-bottom (get top-polyline 0))
      (cons first-bottom top-polyline)
      top-polyline)))

(defn- close-polyline [polyline]
  (if (not= (get polyline 0) (peek polyline))
    (conj polyline (get polyline 0))
    polyline))

(defn- bottom-patch-polyline [top-polyline bottom-polyline]
  (let [last-top (peek top-polyline)]
    ;; polylines not connected at end
    (if (not= last-top (peek bottom-polyline))
      (conj (rseq bottom-polyline) last-top)
      (rseq bottom-polyline))))

(defn corrugated-panel
  "Generated corrugated-panel from descriptor"
  [top-patch bottom-patch
   & {:keys [skin-resolution layer-height corrugation-size modulation-size
             core-line-width skin-line-width corrugate-fn overlap]
      :or   {skin-resolution  200
             corrugate-fn     uniform-corrugations
             layer-height     1/5
             corrugation-size 15
             modulation-size  (* corrugation-size 6)
             core-line-width  4/10
             skin-line-width  3/10}}]
  (let [overlap    (or overlap
                       (calculate-skin-core-overlap skin-line-width core-line-width))
        height     (- (u/round ((protocols/patch-point top-patch 1 0) 2))
                      (u/round ((protocols/patch-point bottom-patch 0 0) 2)))
        layers     (int (/ height layer-height))
        modulation (sine-curve 1 (/ height modulation-size))
        slices     (map (fn [t]
                          (let [top-polyline       (protocols/polyline
                                                    (protocols/patch-slice top-patch t) skin-resolution)
                                bottom-polyline    (protocols/polyline
                                                    (protocols/patch-slice bottom-patch t) skin-resolution)
                                outer-skin-length  (u/polyline-length top-polyline)]
                            [(polyline->layer-slice (top-patch-polyline top-polyline bottom-polyline))
                             (polyline->layer-slice (bottom-patch-polyline top-polyline bottom-polyline))
                             (polyline->layer-slice
                              (horizontal-corrugated-infill
                               top-polyline
                               bottom-polyline
                               (corrugate-fn (/ outer-skin-length corrugation-size)
                                             :shift (protocols/curve-point modulation t))
                               :connecting-distance overlap))]))
                        (layer-interval-sequence layers))]
    {:slices-descriptor [{:source (map #(get % 0) slices)
                          :line-width skin-line-width
                          :layer-fn alternate-direction}
                         {:source (map #(get % 1) slices)
                          :line-width skin-line-width
                          :layer-fn alternate-direction}
                         {:source (map #(get % 2) slices)
                          :line-width core-line-width
                          :connection-move :none
                          :layer-fn alternate-direction}]}))

(defn corrugated-panel-descriptor
  "Generates print descriptor for corrugated sandwich panel"
  [top-patch bottom-patch corrugations layers resolution
   & {:keys [corrugate-fn skin-line-width core-line-width modulate-curve]
      :or {corrugate-fn    uniform-corrugations
           skin-line-width 0.3
           core-line-width 0.4}}]
  (let [slices (map (fn [t]
                      (let [top-polyline       (protocols/polyline
                                                (protocols/patch-slice top-patch t) resolution)
                            bottom-polyline    (protocols/polyline
                                                (protocols/patch-slice bottom-patch t) resolution)]
                        [(polyline->layer-slice (top-patch-polyline top-polyline bottom-polyline))
                         (polyline->layer-slice (bottom-patch-polyline top-polyline bottom-polyline))
                         (polyline->layer-slice
                          (horizontal-corrugated-infill
                           top-polyline
                           bottom-polyline
                           (if modulate-curve
                             (corrugate-fn corrugations :shift (protocols/curve-point modulate-curve t))
                             (corrugate-fn corrugations))
                           :connecting-distance (calculate-skin-core-overlap skin-line-width
                                                                             core-line-width)))]))
                    (layer-interval-sequence layers))]
    {:slices-descriptor [{:source (map #(get % 0) slices)
                          :line-width skin-line-width
                          :layer-fn alternate-direction}
                         {:source (map #(get % 1) slices)
                          :line-width skin-line-width
                          :layer-fn alternate-direction}
                         {:source (map #(get % 2) slices)
                          :line-width core-line-width
                          :connection-move :none
                          :layer-fn alternate-direction}]}))

(defn- mirror-distribution [distribution]
  (reverse (map #(update % 0 not) distribution)))

(defn double-corrugated-panel-descriptor
  "Generates print descriptor for double corrugated sandwich panel"
  [top-patch bottom-patch corrugations layers resolution
   & {:keys [corrugate-fn skin-line-width core-line-width modulate-curve]
      :or {corrugate-fn    uniform-corrugations
           skin-line-width 0.3
           core-line-width 0.4}}]
  (let [connections-distribution (corrugate-fn corrugations)
        connecting-distance      (calculate-skin-core-overlap skin-line-width core-line-width)
        slices (map (fn [t]
                      (let [top-polyline       (protocols/polyline
                                                (protocols/patch-slice top-patch t) resolution)
                            bottom-polyline    (protocols/polyline
                                                (protocols/patch-slice bottom-patch t) resolution)]
                        [(polyline->layer-slice (top-patch-polyline top-polyline bottom-polyline))
                         (polyline->layer-slice (bottom-patch-polyline top-polyline bottom-polyline))
                         (polyline->layer-slice
                          (horizontal-corrugated-infill top-polyline
                                                        bottom-polyline
                                                        (cond-> connections-distribution
                                                          modulate-curve
                                                          (modulate-connections-distribution modulate-curve t))
                                                        :connecting-distance connecting-distance))
                         (polyline->layer-slice
                          (horizontal-corrugated-infill top-polyline
                                                        bottom-polyline
                                                        (cond-> (mirror-distribution connections-distribution)
                                                          modulate-curve
                                                          (modulate-connections-distribution modulate-curve t))
                                                        :connecting-distance connecting-distance))]))
                    (layer-interval-sequence layers))]
    {:slices-descriptor [{:source (map #(get % 0) slices)
                          :line-width skin-line-width}
                         {:source (map #(get % 1) slices)
                          :line-width skin-line-width}
                         {:source (map #(get % 2) slices)
                          :line-width core-line-width
                          :connection-move :travel}
                         {:source (map #(get % 3) slices)
                          :line-width core-line-width
                          :connection-move :travel}]}))

(defn ribbed-corrugated-panel
  "Generated ribbed corrugated panel from descriptor"
  [top-patch bottom-patch
   & {:keys [layer-height skin-resolution skin-line-width
             corrugation-line-width corrugation-size corrugation-overlap corrugate-fn
             ribbing-line-width ribbing-size ribbing-overlap]
      :or   {layer-height            1/5
             skin-line-width         3/10
             skin-resolution         200
             corrugate-fn            uniform-corrugations
             corrugation-size        15
             corrugation-line-width  4/10
             ribbing-size            30
             ribbing-line-width      4/10}}]
  (let [corrugation-overlap          (or corrugation-overlap
                                         (calculate-skin-core-overlap skin-line-width corrugation-line-width))
        ribbing-overlap              (or ribbing-overlap
                                         (calculate-skin-core-overlap skin-line-width ribbing-line-width))
        height                       (- (u/round ((protocols/patch-point top-patch 1 0) 2))
                                        (u/round ((protocols/patch-point bottom-patch 0 0) 2)))
        layers                       (int (/ height layer-height))
        rib-connections-distribution (uniform-corrugations (int (/ height ribbing-size)))
        ribs-index                   (vertical-infill-index top-patch bottom-patch
                                                            skin-resolution rib-connections-distribution
                                                            :connecting-distance ribbing-overlap)
        slices     (map (fn [t]
                          (let [top-polyline       (protocols/polyline
                                                    (protocols/patch-slice top-patch t) skin-resolution)
                                bottom-polyline    (protocols/polyline
                                                    (protocols/patch-slice bottom-patch t) skin-resolution)
                                outer-skin-length  (u/polyline-length top-polyline)]
                            [(polyline->layer-slice top-polyline)
                             (polyline->layer-slice (u/search-range-tree ribs-index t))
                             (polyline->layer-slice
                              (rseq
                               (horizontal-corrugated-infill
                                top-polyline
                                bottom-polyline
                                (corrugate-fn (/ outer-skin-length corrugation-size))
                                :connecting-distance corrugation-overlap)))]))
                        (layer-interval-sequence layers))]
    {:slices-descriptor [{:source (map #(get % 0) slices)
                          :line-width skin-line-width
                          :connection-move :print
                          ;;:layer-fn alternate-direction
                          }
                         {:source (map #(get % 1) slices)
                          :line-width ribbing-line-width
                          ;;:layer-fn alternate-direction
                          }
                         {:source (map #(get % 2) slices)
                          :line-width corrugation-line-width
                          ;;:connection-move :none
                          ;;:layer-fn alternate-direction
                          }]}))

(defn ribbed-panel-descriptor
  "Generates print descriptor for ribbed panel"
  [top-patch bottom-patch corrugations layers resolution
   & {:keys [corrugate-fn skin-line-width core-line-width]
      :or {skin-line-width 0.3
           core-line-width 0.5}}]
  (let [connecting-distance      (calculate-skin-core-overlap skin-line-width core-line-width)
        connections-distribution (uniform-corrugations corrugations)
        ribs-index               (vertical-infill-index top-patch bottom-patch
                                                        resolution connections-distribution
                                                        :connecting-distance connecting-distance)
        slices (map (fn [t]
                      (let [top-polyline        (protocols/polyline
                                                 (protocols/patch-slice top-patch t) resolution)]
                        [(polyline->layer-slice top-polyline)
                         (polyline->layer-slice (rseq (u/search-range-tree ribs-index t)))]))
                    (layer-interval-sequence layers))]
    {:slices-descriptor [{:source (map #(get % 0) slices)
                          :line-width skin-line-width
                          :connection-move :print}
                         {:source (map #(get % 1) slices)
                          :line-width core-line-width
                          :connection-move :print}]}))

(defn closed-ribbed-panel-descriptor
  "Generates print descriptor for closed ribbed panel"
  [top-patch bottom-patch corrugations layers resolution
   & {:keys [corrugate-fn skin-line-width core-line-width]
      :or {corrugate-fn    uniform-corrugations
           skin-line-width 0.3
           core-line-width 0.5}}]
  (let [connecting-distance      (calculate-skin-core-overlap skin-line-width core-line-width)
        connections-distribution (corrugate-fn corrugations)
        ribs-index               (vertical-infill-index top-patch bottom-patch
                                                        resolution connections-distribution
                                                        :connecting-distance connecting-distance)
        slices (map (fn [t]
                      (let [top-polyline    (protocols/polyline
                                             (protocols/patch-slice top-patch t) resolution)
                            bottom-polyline (protocols/polyline
                                             (protocols/patch-slice bottom-patch t) resolution)]
                        [(polyline->layer-slice (top-patch-polyline top-polyline bottom-polyline))
                         (polyline->layer-slice (bottom-patch-polyline top-polyline bottom-polyline))
                         (polyline->layer-slice (u/search-range-tree ribs-index t))]))
                    (layer-interval-sequence layers))]
    {:slices-descriptor [{:source (map #(get % 0) slices)
                          :line-width skin-line-width
                          :layer-fn alternate-direction}
                         {:source (map #(get % 1) slices)
                          :line-width skin-line-width
                          :layer-fn alternate-direction}
                         {:source (map #(get % 2) slices)
                          :line-width core-line-width
                          :connection-move :none
                          :layer-fn alternate-direction}]}))

(defn ribbed-corrugated-panel-descriptor
  "Generates print descriptor for ribbed-corrugated panel"
  [top-patch bottom-patch rib-corrugations corrugations layers resolution
   & {:keys [corrugate-fn skin-line-width core-line-width]
      :or {corrugate-fn    uniform-corrugations
           skin-line-width 0.3
           core-line-width 0.5}}]
  (let [connecting-distance          (calculate-skin-core-overlap skin-line-width core-line-width)
        rib-connections-distribution (uniform-corrugations rib-corrugations)
        ribs-index                   (vertical-infill-index top-patch bottom-patch
                                                            resolution rib-connections-distribution
                                                            :connecting-distance connecting-distance)
        connections-distribution     (corrugate-fn corrugations)
        slices (map (fn [t]
                      (let [top-polyline    (protocols/polyline
                                             (protocols/patch-slice top-patch t) resolution)
                            bottom-polyline (protocols/polyline
                                             (protocols/patch-slice bottom-patch t) resolution)]
                        [(polyline->layer-slice top-polyline)
                         (polyline->layer-slice (rseq (u/search-range-tree ribs-index t)))
                         (polyline->layer-slice
                          (horizontal-corrugated-infill top-polyline
                                                        bottom-polyline
                                                        connections-distribution
                                                        :connecting-distance connecting-distance))]))
                    (layer-interval-sequence layers))]
    {:slices-descriptor [{:source (map #(get % 0) slices)
                          :line-width skin-line-width
                          :connection-move :print
                          :layer-fn alternate-direction}
                         {:source (map #(get % 1) slices)
                          :line-width core-line-width
                          :connection-move :print
                          :layer-fn alternate-direction}
                         {:source (map #(get % 2) slices)
                          :line-width core-line-width
                          :connection-move :print
                          :layer-fn alternate-direction}]}))

