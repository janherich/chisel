(ns chisel.gcode-layers
  "Gcode layer implementations"
  (:require [chisel.protocols :as protocols]
            [chisel.curves :as curves]
            [chisel.coordinates :as c]
            [chisel.utils :as u]))

(def p1
  (curves/bezier-patch
   [(curves/bezier-curve [(c/v [-50 0 0]) (c/v [-50 0 100])])
    (curves/bezier-curve [(c/v [50 0 0]) (c/v [50 0 100])])]))

(def p2
  (curves/bezier-patch
   [(curves/bezier-curve [(c/v [-50 10 0]) (c/v [-50 10 100])])
    (curves/bezier-curve [(c/v [50 10 0]) (c/v [50 10 100])])]))

(defn- alternate-direction [segment layer-idx]
  (cond-> segment
    (odd? layer-idx) (update :polyline reverse)))

(defn- decreasing-line-width [layers-count start target]
  (let [step (/ (- target start) layers-count)]
    (fn [segment layer-idx]
      (assoc segment :line-width (+ start (* step layer-idx))))))

#_(defn vertical-corrugated-infill
  "Given two facesheet polylines, generates vertical corrugated infill between them,
  optionally takes connecting distance argument (defaults to `0.25`)."
  [top-polyline bottom-polyline corrugations t & {:keys [connecting-distance] :or {connecting-distance 0.25}}]
  (let []))

(defn horizontal-corrugated-infill
  "Given two facesheet polylines, generates horizontal corrugated infill between them, 
  optionally takes connecting distance argument (defaults to `0.25`)."
  [top-polyline bottom-polyline corrugations & {:keys [connecting-distance] :or {connecting-distance 0.25}}]
  (let [step                 (/ 1 corrugations 2)
        top-uniform-curve    (curves/uniform-curve top-polyline)
        bottom-uniform-curve (curves/uniform-curve bottom-polyline)]
    (mapv (fn [t top?]
            (let [top-point    (protocols/curve-point top-uniform-curve t)
                  bottom-point (protocols/curve-point bottom-uniform-curve t)]
              (if top?
                (c/sum top-point
                       (c/scale-vector (c/difference bottom-point top-point) connecting-distance))
                (c/sum bottom-point
                       (c/scale-vector (c/difference top-point bottom-point) connecting-distance)))))
          (range 0 (inc step) step)
          (iterate not true))))

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

(defn corrugated-panel-descriptor
  "Generates print descriptor for corrugated sandwich panel"
  [top-patch bottom-patch corrugations layers resolution]
  (let [slices (map (fn [t]
                      (let [top-polyline    (protocols/polyline
                                             (protocols/patch-slice top-patch t) resolution)
                            bottom-polyline (protocols/polyline
                                             (protocols/patch-slice bottom-patch t) resolution)]
                        [(polyline->layer-slice top-polyline)
                         (polyline->layer-slice bottom-polyline)
                         (polyline->layer-slice
                          (horizontal-corrugated-infill top-polyline bottom-polyline corrugations))]))
                    (layer-interval-sequence layers))]
    {:slices-descriptor [{:source (map #(get % 0) slices)
                          :line-width 0.3
                          :connection-move :print
                          :layer-fn alternate-direction}
                         {:source (map #(get % 1) slices)
                          :line-width 0.3
                          :connection-move :print
                          :reversed? true
                          :layer-fn alternate-direction}
                         {:source (map #(get % 2) slices)
                          :connection-move :none
                          :layer-fn alternate-direction}]}))

