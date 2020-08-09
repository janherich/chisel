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

(defn- parameter-assertion [t]
  (assert (>= 1 t 0)
          (format "parameter `t` has to be in (inclusive) range [0 1], :t = %s" t)))

(defrecord BezierCurve [control-points]
  protocols/PParametricCurve
  (curve-point [_ t]
    (parameter-assertion t)
    (c/project (de-casteljau t control-points)))
  (polyline [_ points-count]
    (resolve-curve points-count #(c/project (de-casteljau % control-points))))
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
    (parameter-assertion t)
    (resolve-composite-curve-point t control-points-sequence))
  (polyline [_ points-count]
    (resolve-curve points-count #(resolve-composite-curve-point % control-points-sequence)))
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
    (parameter-assertion t)
    (resolve-bspline-point t span-cp-tuples effective-span))
  (polyline [_ points-count]
    (resolve-curve points-count #(resolve-bspline-point % span-cp-tuples effective-span)))
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
  (patch-point [_ i j]
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
   {:slice-fn       (fn [curves i]
                      (slice-curve-fn (map (fn [curve]
                                             (let [weight (:weight (meta curve))]
                                               (cond-> (protocols/curve-point curve i)
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
  ([patch [i-from i-to] [j-from j-to]]
   (parameter-assertion i-from)
   (parameter-assertion i-to)
   (parameter-assertion j-from)
   (parameter-assertion j-to)
   (assert (> i-to i-from) "start of the `i-range` must be lower then end")
   (assert (> j-to j-from) "start of the `j-range` must be lower then end")
   (reify protocols/PParametricPatch
     (patch-point [_ i j]
       (let [i-transformed (+ i-from (* i (- i-to i-from)))
             j-transformed (+ j-from (* j (- j-to j-from)))]
         (protocols/patch-point patch i-transformed j-transformed))))))

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
