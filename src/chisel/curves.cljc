(ns chisel.curves
  "Various curve/patch implementations"
  (:require [clojure.set :as s]
            [chisel.protocols :as protocols]
            [chisel.coordinates :as c]
            [chisel.utils :as u]))

(defrecord ConstantCurve [point]
  protocols/PParametricCurve
  (curve-point [_ _] point)
  (polyline [_ points-count] (repeat points-count point))
  (closed? [_] true)
  protocols/PTransformable
  (linear-transform [_ matrix]
    (update :point #(protocols/linear-transform % matrix))))

(defn- control-point-assertion [cp]
  (assert (c/valid-point? cp) "control point must be a vector of numbers with size 4"))

(defn constant-curve
  "Constant curve, which always resolves to fixed point.
  Useful when creating degenerate surface patches, etc."
  [point]
  (control-point-assertion point)
  (ConstantCurve. point))

(defn de-casteljau
  "De Casteljau algorithm for recursively calculating Bezier curve point 
  from parameter `t` and sequence of `control-points`"
  [t [point-1 point-2 :as control-points]]
  (if (= 2 (count control-points))
    (c/linear-combination t point-1 point-2)
    (de-casteljau t (map (partial de-casteljau t) (partition 2 1 control-points)))))

(defn- resolve-curve-xf
  "Transducer resolving parametric curve function by mapping [0-1] interval to the function,
  divided equally into `points-count` fractions."
  [points-count curve-fn]
  (assert (> points-count 1) "curve needs at least 2 points")
  (let [denominator (dec points-count)]
    (map #(curve-fn (/ % denominator)))))

(defn- resolve-curve
  [points-count curve-fn]
  (into [] (resolve-curve-xf points-count curve-fn) (range points-count)))

(defrecord BezierCurve [control-points]
  protocols/PParametricCurve
  (curve-point [_ t]
    (u/relative-parameter-assertion t)
    (c/project (de-casteljau t control-points)))
  (polyline [this points-count]
    (resolve-curve points-count #(protocols/curve-point this %)))
  (closed? [_]
    (= (first control-points) (last control-points)))
  protocols/PTransformable
  (linear-transform [this matrix]
    (update this :control-points #(map (fn [cp] (protocols/linear-transform cp matrix)) %))))

(defn- control-points-assertions [control-points]
  (assert (> (count control-points) 1)
          "at least two `control-points` are required for Bezier curve")
  (doall (map control-point-assertion control-points)))

(defn bezier-curve
  "Creates new bezier curve"
  [control-points]
  (control-points-assertions control-points)
  (->BezierCurve control-points))

(defn- resolve-composite-curve-point
  [t control-points-sequence]
  (let [cps-ratio                (/ 1 (count control-points-sequence))
        [control-points local-t] (if (= t 1)
                                   [(last control-points-sequence) 1]
                                   [(nth control-points-sequence (quot t cps-ratio))
                                    (/ (rem t cps-ratio) cps-ratio)])]
    (c/project (de-casteljau local-t control-points))))

(defrecord CompositeBezierCurve [control-points-sequence]
  protocols/PParametricCurve
  (curve-point [_ t]
    (u/relative-parameter-assertion t)
    (resolve-composite-curve-point t control-points-sequence))
  (polyline [this points-count]
    (resolve-curve points-count #(protocols/curve-point this %)))
  (closed? [_]
    (= (ffirst control-points-sequence)
       (last (last control-points-sequence))))
  protocols/PTransformable
  (linear-transform [this matrix]
    (update this :control-points-sequence
            #(map (fn [control-points]
                    (map (fn [cp] (protocols/linear-transform cp matrix))
                         control-points)) %))))

(defn- control-points-sequence-assertions [control-points-sequence]
  (doall (map control-points-assertions control-points-sequence))
  (when (> (count control-points-sequence) 1)
    (assert (= (rest (map first control-points-sequence))
               (butlast (map last control-points-sequence)))
            "control points in the `control-points-sequence` need to be connected")))

(defn composite-bezier-curve
  "Creates new composite bezier curve"
  [control-points-sequence]
  (control-points-sequence-assertions control-points-sequence)
  (->CompositeBezierCurve control-points-sequence))

(defn- parameter->effective-curve
  "Given parameter value t, returns sequence of `span-cp-tuples` necessary for computation of t"
  [t span-cp-tuples]
  (let [compare (if (or (zero? t) (or (= t 1) (= t 1.0))) >= >)]
    (filter (fn [[[from to] _]]
              (compare to t from))
            span-cp-tuples)))

(defn- de-boor
  [t [[[_ to] point-1] [[from] point-2] :as span-cp-tuples]]
  (cond
    (= 2 (count span-cp-tuples))
    (let [coefficient (/ (- t from) (- to from))]
      [[from to]
       (c/linear-combination coefficient point-1 point-2)])
    (= 1 (count span-cp-tuples))
    (first span-cp-tuples)
    :else
    (de-boor t (map (partial de-boor t) (partition 2 1 span-cp-tuples)))))

(defn- resolve-bspline-point [t span-cp-tuples [from to]]
  (let [effective-t (+ (* t (- to from)) from)]
    (->> (parameter->effective-curve effective-t span-cp-tuples)
         (de-boor effective-t)
         second
         c/project)))

(defrecord BSplineCurve [span-cp-tuples effective-span]
  protocols/PParametricCurve
  (curve-point [_ t]
    (u/relative-parameter-assertion t)
    (resolve-bspline-point t span-cp-tuples effective-span))
  (polyline [this points-count]
    (resolve-curve points-count #(protocols/curve-point this %)))
  (closed? [_]
    (and (= [0 1] effective-span)
         (= (last (first span-cp-tuples))
            (last (last span-cp-tuples)))))
  protocols/PTransformable
  (linear-transform [this matrix]
    (update this :span-cp-tuples
            #(mapv (fn [[span cp]] [span (protocols/linear-transform cp matrix)]) %))))

(defn- b-spline-assertions [{:keys [control-points knot-vector order]}]
  (let [control-points-count (count control-points)
        required-knot-vector-count (+ control-points-count order 1)]
    (control-points-assertions control-points)
    (assert (> order 0)
            "Order of the B-Spline has to be at least 1")
    (assert (> control-points-count order)
            (str "B-spline of the order " order " needs at least " (inc order) " control points"))
    (assert (= (count knot-vector) required-knot-vector-count)
            (str "B-spline of with " control-points-count " control points of order " order
                 " needs to have knot-vector with " required-knot-vector-count " elements"))
    (assert (and (= 0 (first knot-vector))
                 (= 1 (last knot-vector))
                 (every? (fn [[a b]] (<= a b)) (partition 2 1 knot-vector)))
            "Knot vector needs to be non-decreasing sequence of numbers, normalized on [0 1] interval")))

(defn- process-b-spline [{:keys [control-points knot-vector order]}]
  {:span-cp-tuples (map vector
                        (map (juxt first last) (partition (+ order 2) 1 knot-vector))
                        control-points)
   :effective-span [(nth knot-vector order) (nth knot-vector (count control-points))]})

(defn b-spline
  "Creates new b-spline curve"
  [opts]
  (b-spline-assertions opts)
  (map->BSplineCurve (process-b-spline opts)))

(defn- clamp-knot-vector
  [{:keys [order] :as opts}]
  (update opts :knot-vector
          #(into [] (concat (repeat (inc order) 0)
                            %
                            (repeat (inc order) 1)))))

(defn clamped-b-spline
  "Creates new clamped b-spline curve"
  [opts]
  (b-spline (clamp-knot-vector opts)))

(defn clamped-uniform-b-spline
  "Creates new clamped b-spline curve with uniform knot vector"
  [{:keys [control-points order] :as opts}]
  (let [upper-limit (- (count control-points) order)
        knot-vector (mapv #(/ % upper-limit) (range 1 upper-limit))]
    (b-spline (clamp-knot-vector (assoc opts :knot-vector knot-vector)))))

(defn- search-tree [{:keys [range lower-range upper-range interpolate-fn]} polyline t]
  (let [[start end] range]
    (cond
      (< t start) (search-tree lower-range polyline t)
      (> t end)   (search-tree upper-range polyline t)
      :else       (interpolate-fn t polyline))))

(defrecord UniformCurve [query-tree polyline closed?]
  protocols/PParametricCurve
  (curve-point [_ t]
    (u/relative-parameter-assertion t)
    (search-tree query-tree polyline t))
  (polyline [this points-count]
    (resolve-curve points-count #(protocols/curve-point this %)))
  (closed? [_] closed?)
  protocols/PTransformable
  (linear-transform [this matrix]
    (update this :polyline
            #(mapv (fn [p] (protocols/linear-transform p matrix)) %))))

(def ^:private axis->idx {:x 0 :y 1 :z 2})

(defn- tree-node [ranges-vector]
  (let [ranges-count      (count ranges-vector)
        median-index      (int (/ ranges-count 2))
        median-plus-index (inc median-index)]
    (cond-> (get ranges-vector median-index)
      (< 0 median-index)
      (assoc :lower-range (tree-node (subvec ranges-vector 0 median-index)))
      (< median-plus-index ranges-count)
      (assoc :upper-range (tree-node (subvec ranges-vector median-plus-index ranges-count))))))

(defn axis-uniform-curve
  "Creates curve from polyline vector providing uniform distribution of points along given axis.
  Polyline must be monotonically increasing/decreasing along given axis."
  [polyline axis]
  (let [axis-getter    (axis->idx axis)
        first-point    (first polyline)
        last-point     (peek polyline)
        min-axis-value (first-point axis-getter)
        max-axis-value (last-point axis-getter)
        axis-range     (- max-axis-value min-axis-value)
        increasing?    (pos? axis-range)
        ranges         (into []
                             (map-indexed (fn [idx [start-point end-point]]
                                            (let [start-range (/ (- (start-point axis-getter) min-axis-value)
                                                                 axis-range)
                                                  end-range   (/ (- (end-point axis-getter) min-axis-value)
                                                                 axis-range)
                                                  diff        (- end-range start-range)]
                                              (if increasing?
                                                (assert (pos? diff) "Polyline must be increasing")
                                                (assert (neg? diff) "Polyline must be decreasing"))
                                              {:range          [start-range end-range]
                                               :interpolate-fn (fn [t polyline]
                                                                 (c/linear-combination (/ (- t start-range)
                                                                                          diff)
                                                                                       (get polyline idx)
                                                                                       (get polyline (inc idx))))})))
                             (partition 2 1 polyline))]
    (map->UniformCurve {:query-tree (tree-node ranges)
                        :closed?    (= first-point last-point)
                        :polyline   polyline})))

(defn uniform-curve
  "Creates curve from polyline vector providing uniform distribution of points."
  [polyline]
  (let [overall-length (u/polyline-length polyline)
        ranges         (first
                        (reduce (fn [[ranges current-length idx] line]
                                  (let [new-length  (+ current-length (u/line-length line))
                                        start-range (/ current-length overall-length)
                                        end-range   (/ new-length overall-length)
                                        diff        (- end-range start-range)]
                                    [(conj ranges {:range          [start-range end-range]
                                                   :interpolate-fn (fn [t polyline]
                                                                     (c/linear-combination (/ (- t start-range)
                                                                                              diff)
                                                                                           (get polyline idx)
                                                                                           (get polyline (inc idx))))})
                                     new-length
                                     (inc idx)]))
                                [[] 0 0]
                                (partition 2 1 polyline)))]
    (map->UniformCurve {:query-tree (tree-node ranges)
                        :closed?    (= (first polyline) (peek polyline))
                        :polyline   polyline})))

(defn cut-curve
  "Cuts parametric curve to subselection defined in terms of [0 1] parameter range"
  [curve [from to]]
  (u/relative-parameter-assertion from)
  (u/relative-parameter-assertion to)
  (assert (> to from) "Start of the parameter range must be lower then end")
  (letfn [(transform [t] (+ from (* t (- to from))))]
    (reify
      protocols/PParametricCurve
      (curve-point [_ t]
        (protocols/curve-point curve (transform t)))
      (polyline [this points-count]
        (resolve-curve points-count #(protocols/curve-point this %)))
      (closed? [_] false)
      protocols/PTransformable
      (linear-transform [_ matrix]
        (cut-curve (protocols/linear-transform curve matrix) [from to])))))

(defn- resolve-patch-points
  "Resolves patch points based on i-j resolution + slice function"
  [i-count j-count slice-fn]
  (transduce (resolve-curve-xf
              i-count
              (fn [i]
                (let [curve (slice-fn i)]
                  (resolve-curve j-count (partial protocols/curve-point curve)))))
             into
             (range i-count)))

(defn- bindable-perimeter-curves [curves]
  (let [curves-set (into #{} curves)]
    (when (> (count curves-set) 1)
      (remove #(instance? ConstantCurve %) curves-set))))

(defn- perimeter-curves [i-curves j-curves]
  (let [i-bindable (bindable-perimeter-curves i-curves)
        j-bindable (bindable-perimeter-curves j-curves)]
    (cond-> {}
      i-bindable (assoc i-bindable :i)
      j-bindable (assoc j-bindable :j))))

(defrecord TensorProductPatch [slice-fn control-curves]
  protocols/PParametricPatch
  (patch-slice [this t]
    (slice-fn control-curves t))
  (patch-point [this i j]
    (protocols/curve-point (slice-fn control-curves i) j))
  protocols/PPatch
  (triangle-mesh [_ [i-count j-count]]
    {:points (resolve-patch-points i-count j-count (partial slice-fn control-curves))
     :faces  (u/triangulate-rectangular-mesh i-count j-count)})
  (perimeter-curves [_]
    (perimeter-curves [(first control-curves) (last control-curves)]
                      [(slice-fn control-curves 0) (slice-fn control-curves 1)]))
  protocols/PTransformable
  (linear-transform [this matrix]
    (update this :control-curves #(map (fn [cc] (protocols/linear-transform cc matrix)) %))))

(defn tensor-product-patch
  "Creates new tensor product patch"
  [slice-curve-fn control-curves]
  (map->TensorProductPatch
   {:slice-fn       (fn [curves t]
                      (slice-curve-fn (map (fn [curve]
                                             (let [weight (:weight (meta curve))]
                                               (cond-> (protocols/curve-point curve t)
                                                 weight (c/weighted weight))))
                                           curves)))
    :control-curves control-curves}))

(defn bezier-patch
  "Creates new bezier patch"
  [control-curves]
  (tensor-product-patch bezier-curve control-curves))

(defn b-spline-patch
  "Creates new b-splie-patch"
  [{:keys [control-curves] :as opts}]
  (tensor-product-patch (fn [points]
                          (b-spline (assoc opts :control-points points)))
                        control-curves))

(defn clamped-b-spline-patch
  "Creates new b-splie-patch"
  [{:keys [control-curves] :as opts}]
  (let [clamped-opts (clamp-knot-vector opts)]
    (tensor-product-patch (fn [points]
                            (b-spline (assoc clamped-opts :control-points points)))
                          control-curves)))

(defn clamped-uniform-b-spline-patch
  "Creates new b-splie-patch"
  [{:keys [control-curves order] :as opts}]
  (let [upper-limit  (- (count control-curves) order)
        knot-vector  (mapv #(/ % upper-limit) (range 1 upper-limit))
        clamped-opts (clamp-knot-vector (assoc opts :knot-vector knot-vector))]
    (tensor-product-patch (fn [points]
                            (b-spline (assoc clamped-opts :control-points points)))
                          control-curves)))

(defn- join-paths [{paths :perimeter-curve->fabric-path :as acc} {:keys [direction edges patch]}]
  (let [[join-edge path-to] edges
        join-start          (get paths join-edge)
        join-end            (get paths path-to)
        start               (if join-start (:path-to join-start) join-edge)
        end                 (if join-end (:path-to join-end) path-to)
        new-direction       (or (:direction join-start) (:direction join-end) direction)
        closed-path?        (or (= start end)
                                (= path-to start)
                                (= join-edge end))] (-> acc
        (assoc :perimeter-curve->fabric-path (cond-> (dissoc paths join-edge path-to)
                                               (and end (not closed-path?))
                                               (assoc end (cond-> {:direction new-direction}
                                                            start (assoc :path-to start)))
                                               (and start (not closed-path?))
                                               (assoc start (cond-> {:direction new-direction}
                                                              end (assoc :path-to end)))))
        (assoc-in [:patches patch direction] new-direction))))

(defn- stitch-patch [stitched-patch patch]
  (let [perimeter-curves (protocols/perimeter-curves patch)]
    (reduce-kv (fn [acc edges direction]
                 (join-paths acc {:direction direction
                                  :edges     edges
                                  :patch     patch}))
               stitched-patch
               perimeter-curves)))

(defrecord StitchedPatch [perimeter-curve->fabric-path patches]
  protocols/PPatch
  (triangle-mesh [_ [i-count j-count]]
    (let [direction-base {:i i-count
                          :j j-count}]
      (reduce u/merge-triangle-meshes
              {:points []
               :faces  []}
              (pmap (fn [[patch direction-map]]
                      (protocols/triangle-mesh patch
                                               [(get direction-base (get direction-map :i :i))
                                                (get direction-base (get direction-map :j :j))]))
                    patches))))
  (perimeter-curves [_]
    (reduce-kv (fn [acc from {:keys [direction path-to]}]
                 (assoc acc (cond-> [from] path-to (conj path-to)) direction))
               {}
               perimeter-curve->fabric-path))
  protocols/PStichable
  (stitch [this new-patch]
    (stitch-patch this new-patch)))

(defn stitched-patch
  "Creates new stitched patch"
  [& patches]
  (reduce protocols/stitch
          (map->StitchedPatch {:perimeter-curve->fabric-path {}
                               :patches                      {}})
          patches))

(defn cut-patch
  "Cuts parametric patch to subselection defined in terms of i/j ranges"
  ([patch i-range]
   (cut-patch patch i-range [0 1]))
  ([patch i-range j-range]
   (-> patch
       (update :control-curves #(map (fn [curve]
                                       (with-meta (cut-curve curve i-range) (meta curve))) %))
       (update :slice-fn #(comp (fn [curve]
                                  (cut-curve curve j-range)) %)))))

(defn mapped-curve
  "Map curve to patch"
  [patch curve]
  (reify protocols/PParametricCurve
    (protocols/curve-point [_ t]
      (let [point (protocols/curve-point curve t)]
        (protocols/patch-point patch (point 0) (point 1))))
    (protocols/polyline [_ points-count]
      (map #(protocols/patch-point patch (% 0) (% 1))
           (protocols/polyline curve points-count)))
    (protocols/closed? [_]
      (protocols/closed? curve))))

(comment
  (require '[chisel.open-scad :as os])
  (require '[chisel.stl :as stl])
  ;; Fast surf-ski hull planform
  (os/write
   (os/generate-polygon
    (protocols/polyline
     (composite-bezier-curve [[(c/v [-80 0]) (c/v [-80 2]) (c/v [-10 5]) (c/v [0 5])]
                              [(c/v [0 5]) (c/v [25 5]) (c/v [70 2]) (c/v [70 0])]
                              [(c/v [70 0]) (c/v [70 -2]) (c/v [25 -5]) (c/v [0 -5])]
                              [(c/v [0 -5]) (c/v [-10 -5]) (c/v [-80 -2]) (c/v [-80 0])]])
     1000)))
  ;; Inverted bow yacht hull
  (os/write
   (os/generate-polyhedron
    (protocols/triangle-mesh
     (bezier-patch
      [(bezier-curve [(c/v [5 0 0]) (c/v [-5 0 50]) (c/v [1 0 100])])
       (bezier-curve [(c/v [5 10 -5]) (c/v [-5 12 50]) (c/v [1 5 95])])
       (bezier-curve [(c/v [5 10 -5]) (c/v [15 12 50]) (c/v [9 5 95])])
       (bezier-curve [(c/v [5 0 0]) (c/v [15 0 50]) (c/v [9 0 100])])])
     [100 100])))
  ;; Fast surf-ski hull
  (os/write
   (os/generate-polyhedron
    (protocols/triangle-mesh
     (bezier-patch
      [(composite-bezier-curve
        [[(c/v [-80 0 0]) (c/v [-80 2 0]) (c/v [-10 5 0]) (c/v [0 5 0])]
         [(c/v [0 5 0]) (c/v [25 5 0]) (c/v [70 2 0]) (c/v [70 0 0])]])
       (b-spline {:control-points [(c/v [-80 0 0]) (c/v [-105 1 4]) (c/v [-80 1 5]) (c/v [-10 5 5])
                                   (c/v [0 5 5]) (c/v [25 5 4]) (c/v [70 2 2]) (c/v [70 0 0])]
                  :knot-vector [0 0 0 0 1/5 2/5 3/5 4/5 1 1 1 1]
                  :order 3})
       (b-spline {:control-points [(c/v [-80 0 0]) (c/v [-105 -1 4]) (c/v [-80 -1 5]) (c/v [-10 -5 5])
                                   (c/v [0 -5 5]) (c/v [25 -5 4]) (c/v [70 -2 2]) (c/v [70 0 0])]
                  :knot-vector [0 0 0 0 1/5 2/5 3/5 4/5 1 1 1 1]
                  :order 3})
       (composite-bezier-curve
        [[(c/v [-80 0 0]) (c/v [-80 -2 0]) (c/v [-10 -5 0]) (c/v [0 -5 0])]
         [(c/v [0 -5 0]) (c/v [25 -5 0]) (c/v [70 -2 0]) (c/v [70 0 0])]])])
     [400 100])))
  ;; Fast surf-ski hull take 2
  (os/write
   (os/generate-polyhedron
    (protocols/triangle-mesh
     (bezier-patch
      [(clamped-b-spline {:control-points [(c/v [-80 0 0]) (c/v [-80 1 0]) (c/v [0 5 0]) (c/v [5 5 0])
                                           (c/v [20 5 0]) (c/v [70 1 0]) (c/v [70 0 0])]
                          :knot-vector [1/4 2/4 3/4]
                          :order 3})
       (clamped-b-spline {:control-points [(c/v [-80 0 0]) (c/v [-105 1 4]) (c/v [-80 1 5]) (c/v [-10 5 5])
                                           (c/v [0 5 5]) (c/v [25 5 4]) (c/v [65 2 2]) (c/v [70 1 2]) (c/v [70 0 0])]
                          :knot-vector [1/6 2/6 3/6 4/6 5/6]
                          :order 3})
       (clamped-b-spline {:control-points [(c/v [-80 0 0]) (c/v [-105 -1 4]) (c/v [-80 -1 5]) (c/v [-10 -5 5])
                                           (c/v [0 -5 5]) (c/v [25 -5 4]) (c/v [65 -2 2]) (c/v [70 -1 2]) (c/v [70 0 0])]
                          :knot-vector [1/6 2/6 3/6 4/6 5/6]
                          :order 3})
       (clamped-b-spline {:control-points [(c/v [-80 0 0]) (c/v [-80 -1 0]) (c/v [0 -5 0]) (c/v [5 -5 0])
                                           (c/v [20 -5 0]) (c/v [70 -1 0]) (c/v [70 0 0])]
                          :knot-vector [1/4 2/4 3/4]
                          :order 3})])
     [400 100])))
  ;; Fast surf-ski hull take 2 direct stl generation
  (stl/write
   (stl/generate-ascii-solid
    (protocols/triangle-mesh
      (bezier-patch
       [(clamped-b-spline {:control-points [(c/v [-80 0 0]) (c/v [-80 1 0]) (c/v [0 5 0]) (c/v [5 5 0])
                                            (c/v [20 5 0]) (c/v [70 1 0]) (c/v [70 0 0])]
                           :knot-vector [1/4 2/4 3/4]
                           :order 3})
        (clamped-b-spline {:control-points [(c/v [-80 0 0]) (c/v [-105 1 4]) (c/v [-80 1 5]) (c/v [-10 5 5])
                                            (c/v [0 5 5]) (c/v [25 5 4]) (c/v [65 2 2]) (c/v [70 1 2]) (c/v [70 0 0])]
                           :knot-vector [1/6 2/6 3/6 4/6 5/6]
                           :order 3})
        (clamped-b-spline {:control-points [(c/v [-80 0 0]) (c/v [-105 -1 4]) (c/v [-80 -1 5]) (c/v [-10 -5 5])
                                            (c/v [0 -5 5]) (c/v [25 -5 4]) (c/v [65 -2 2]) (c/v [70 -1 2]) (c/v [70 0 0])]
                           :knot-vector [1/6 2/6 3/6 4/6 5/6]
                           :order 3})
        (clamped-b-spline {:control-points [(c/v [-80 0 0]) (c/v [-80 -1 0]) (c/v [0 -5 0]) (c/v [5 -5 0])
                                            (c/v [20 -5 0]) (c/v [70 -1 0]) (c/v [70 0 0])]
                           :knot-vector [1/4 2/4 3/4]
                           :order 3})])
      [400 100]))))
