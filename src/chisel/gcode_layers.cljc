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

(defn uniform-corrugations
  "Returns uniform corrugation distribution as sequence of `[side t]` tuples,
  where `side` is boolean indicating corrugation face curve and `t` relative point
  on the curve in `[0 1]` interval. "
  [corrugations]
  (corrugations-assertion corrugations)
  (let [step (/ 1 corrugations 2)]
    (map vector
         (iterate not true)
         (range 0 (inc step) step))))

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
          (iterate not true)
          ;; cumbersome take/iterate combinations necessare because range doesn't work well with floating point numbers
          (-> (take (inc (* 2 corrugations))
                    (iterate (partial + step) start))
              (conj start)
              vec
              (conj end))))))

(defn- distance-on-line
  "Given two points and absolute distance, returns point which lies on line between `p1` and `p2`
  and is separated from `p1` by given distance."
  [distance p1 p2]
  (c/sum p1 (c/scale-vector (c/difference p2 p1) distance)))

(defn horizontal-corrugated-infill
  "Given two facesheet polylines and connections-distributing sequence,
  generates horizontal corrugated infill between them, 
  optionally takes connecting distance argument (defaults to `0.3`)."
  [top-polyline bottom-polyline connections-distribution
   & {:keys [connecting-distance] :or {connecting-distance 0.3}}]
  (let [top-uniform-curve    (curves/uniform-curve top-polyline)
        bottom-uniform-curve (curves/uniform-curve bottom-polyline)]
    (mapv (fn [[top? t]]
            (let [top-point    (protocols/curve-point top-uniform-curve t)
                  bottom-point (protocols/curve-point bottom-uniform-curve t)]
              (if top?
                (distance-on-line connecting-distance top-point bottom-point)
                (distance-on-line connecting-distance bottom-point top-point))))
          connections-distribution)))

(defn- layer-interval-sequence [layers]
  (let [layer-step (/ 1 layers)]
    (range 0 (+ 1 layer-step) layer-step)))

(defn- round-z [z]
  (Double. (format "%.3f" (double z))))

(defn- polyline->layer-slice [[first-point & polyline-rest]]
  (let [z (round-z (first-point 2))]
    {:z        z
     :polyline (into [[(first-point 0) (first-point 1)]]
                     (map (fn [point]
                            (let [point-z (round-z (point 2))]
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

(defn sine-curve
  "Returns sine curve with given frequency and amplitude, amplitude in `[0 1]` parameter ranges."
  [amplitude frequency]
  (u/relative-parameter-assertion amplitude)
  (reify protocols/PParametricCurve
    (protocols/curve-point [_ t]
      (u/relative-parameter-assertion t)
      (let [angle-rad (* t frequency (* 2 Math/PI))]
        (* amplitude (Math/sin angle-rad))))
    (protocols/polyline [this points-count]
      (curves/resolve-curve points-count #(protocols/curve-point this %)))
    (protocols/closed? [_] true)))

(defn vertical-infill-index
  "Given two facesheet patches, resolution and connections-distributing sequence,
  generates index (range-tree) returning verticall infill between them,
  optionally takes connecting distance argument (defaults to `0.10`). "
  [top-patch bottom-patch resolution connections-distribution
   & {:keys [connecting-distance] :or {connecting-distance 0.20}}]
  (u/make-range-tree
   (mapv (fn [[[first-top? t-1] [second-top? t-2]]]
           (let [range-diff   (- t-2 t-1)
                 first-patch  (if first-top? top-patch bottom-patch)
                 second-patch (if second-top? top-patch bottom-patch)
                 anchor-1     (map (partial distance-on-line connecting-distance)
                                   (protocols/polyline
                                    (protocols/patch-slice first-patch t-1) resolution)
                                   (protocols/polyline
                                    (protocols/patch-slice second-patch t-1) resolution))
                 anchor-2     (map (partial distance-on-line connecting-distance)
                                   (protocols/polyline
                                    (protocols/patch-slice second-patch t-2) resolution)
                                   (protocols/polyline
                                    (protocols/patch-slice first-patch t-2) resolution))]  
             {:range          [t-1 t-2]
              :interpolate-fn (fn [t]
                                (mapv (fn [p1 p2]
                                        (c/linear-combination (/ (- t t-1) range-diff) p1 p2))
                                      anchor-1 anchor-2))}))
         (partition 2 1 connections-distribution))))

(defn corrugated-panel-descriptor
  "Generates print descriptor for corrugated sandwich panel"
  [top-patch bottom-patch corrugations layers resolution
   & {:keys [corrugate-fn skin-line-width core-line-width modulate-curve]
      :or {corrugate-fn    uniform-corrugations
           skin-line-width 0.3
           core-line-width 0.5}}]
  (let [connections-distribution (corrugate-fn corrugations)
        slices (map (fn [t]
                      (let [top-polyline       (protocols/polyline
                                                (protocols/patch-slice top-patch t) resolution)
                            bottom-polyline    (protocols/polyline
                                                (protocols/patch-slice bottom-patch t) resolution)]
                        [(polyline->layer-slice top-polyline)
                         (polyline->layer-slice (rseq bottom-polyline))
                         (polyline->layer-slice
                          (horizontal-corrugated-infill top-polyline
                                                        bottom-polyline
                                                        (cond-> connections-distribution
                                                          modulate-curve
                                                          (modulate-connections-distribution modulate-curve t))))]))
                    (layer-interval-sequence layers))]
    {:slices-descriptor [{:source (map #(get % 0) slices)
                          :line-width skin-line-width
                          :connection-move :print #_:travel
                          :layer-fn alternate-direction}
                         {:source (map #(get % 1) slices)
                          :line-width skin-line-width
                          :connection-move :print #_:travel
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
           core-line-width 0.5}}]
  (let [connections-distribution (corrugate-fn corrugations)
        slices (map (fn [t]
                      (let [top-polyline       (protocols/polyline
                                                (protocols/patch-slice top-patch t) resolution)
                            bottom-polyline    (protocols/polyline
                                                (protocols/patch-slice bottom-patch t) resolution)]
                        [(polyline->layer-slice top-polyline)
                         (polyline->layer-slice (rseq bottom-polyline))
                         (polyline->layer-slice
                          (horizontal-corrugated-infill top-polyline
                                                        bottom-polyline
                                                        (cond-> connections-distribution
                                                          modulate-curve
                                                          (modulate-connections-distribution modulate-curve t))))
                         (polyline->layer-slice
                          (horizontal-corrugated-infill top-polyline
                                                        bottom-polyline
                                                        (cond-> (mirror-distribution connections-distribution)
                                                          modulate-curve
                                                          (modulate-connections-distribution modulate-curve t))))]))
                    (layer-interval-sequence layers))]
    {:slices-descriptor [{:source (map #(get % 0) slices)
                          :line-width skin-line-width
                          :connection-move #_print :travel}
                         {:source (map #(get % 1) slices)
                          :line-width skin-line-width
                          :connection-move #_:print :travel}
                         {:source (map #(get % 2) slices)
                          :line-width core-line-width
                          :connection-move :travel}
                         {:source (map #(get % 3) slices)
                          :line-width core-line-width
                          :connection-move :travel}]}))

(defn ribbed-panel-descriptor
  "Generates print descriptor for ribbed panel"
  [top-patch bottom-patch corrugations layers resolution
   & {:keys [corrugate-fn skin-line-width core-line-width]
      :or {corrugate-fn    uniform-corrugations
           skin-line-width 0.3
           core-line-width 0.5}}]
  (let [connections-distribution (corrugate-fn corrugations)
        ribs-index               (vertical-infill-index top-patch bottom-patch
                                                        resolution connections-distribution)
        slices (map (fn [t]
                      (let [top-polyline        (protocols/polyline
                                                 (protocols/patch-slice top-patch t) resolution)]
                        [(polyline->layer-slice top-polyline)
                         (polyline->layer-slice (rseq (u/search-range-tree ribs-index t)))]))
                    (layer-interval-sequence layers))]
    {:slices-descriptor [{:source (map #(get % 0) slices)
                          :line-width skin-line-width
                          :connection-move :travel}
                         {:source (map #(get % 1) slices)
                          :line-width core-line-width
                          :connection-move :travel}]}))

(defn ribbed-corrugated-panel-descriptor
  "Generates print descriptor for ribbed-corrugated panel"
  [top-patch bottom-patch rib-corrugations corrugations layers resolution
   & {:keys [corrugate-fn skin-line-width core-line-width]
      :or {corrugate-fn    uniform-corrugations
           skin-line-width 0.3
           core-line-width 0.5}}]
  (let [rib-connections-distribution (uniform-corrugations rib-corrugations)
        ribs-index                   (vertical-infill-index top-patch bottom-patch
                                                            resolution rib-connections-distribution)
        connections-distribution     (corrugate-fn corrugations)
        slices (map (fn [t]
                      (let [top-polyline        (protocols/polyline
                                                 (protocols/patch-slice top-patch t) resolution)
                            bottom-polyline     (protocols/polyline
                                                 (protocols/patch-slice bottom-patch t) resolution)]
                        [(polyline->layer-slice top-polyline)
                         (polyline->layer-slice (rseq (u/search-range-tree ribs-index t)))
                         (polyline->layer-slice
                          (horizontal-corrugated-infill top-polyline
                                                        bottom-polyline
                                                        connections-distribution))]))
                    (layer-interval-sequence layers))]
    {:slices-descriptor [{:source (map #(get % 0) slices)
                          :line-width skin-line-width
                          :connection-move :travel
                          :layer-fn alternate-direction}
                         {:source (map #(get % 1) slices)
                          :line-width core-line-width
                          :connection-move :travel
                          :layer-fn alternate-direction}
                         {:source (map #(get % 2) slices)
                          :line-width core-line-width
                          :connection-move :travel
                          :layer-fn alternate-direction}]}))
