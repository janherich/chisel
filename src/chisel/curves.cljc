(ns chisel.curves
  "Various curve/patch implementations"
  (:require [chisel.protocols :as protocols]
            [chisel.coordinates :as c]))

(defrecord ConstantCurve [point]
  protocols/PParametricCurve
  (point [_ _] point)
  (points [_ points-count] (repeat points-count point))
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

(defn resolve-curve
  "Resolves parametric curve function by mapping [0-1] interval to the function, divided
  equally into `points-count` fractions. Interval can be optionally left open at start by
  passing `:drop-first?` kw arg or left open at end by passing `:drop-last?` kw arg, it's
  closed on both ends by default."
  [points-count curve-fn & {:keys [drop-first? drop-last?]
                            :or   {drop-first? false drop-last? false}}]
  (assert (> points-count 1) "curve needs at least 2 points")
  (assert (not (and drop-first? drop-last?)) "Either drop-first? or drop-last? is supported, not both")
  (let [denominator (if (or drop-first? drop-last?) points-count (dec points-count))]
    (map (comp curve-fn #(/ % denominator))
         (range (if drop-first? 1 0)
                (if drop-first? (inc points-count) points-count)))))

(defn- parameter-assertion [t]
  (assert (>= 1 t 0) "parameter `t` has to be in (inclusive) range [0 1]"))

(defrecord BezierCurve [control-points]
  protocols/PParametricCurve
  (point [_ t]
    (parameter-assertion t)
    (c/project (de-casteljau t control-points)))
  (points [_ points-count]
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
  (point [_ t]
    (parameter-assertion t)
    (resolve-composite-curve-point t control-points-sequence))
  (points [_ points-count]
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
  (let [compare (if (or (= t 0) (= t 1)) >= >)]
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
  (point [_ t]
    (parameter-assertion t)
    (resolve-bspline-point t span-cp-tuples effective-span))
  (points [_ points-count]
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

(defrecord TensorProductPatch [slice-fn control-curves]
  protocols/PParametricPatch
  (slice-points [_ t slice-points-count]
    (protocols/points
     (slice-fn (map #(protocols/point % t)
                    control-curves))
     slice-points-count))
  (patch-points [this slice-points-count slices-count]
    (resolve-curve slices-count #(protocols/slice-points this % slice-points-count)))
  protocols/PTransformable
  (linear-transform [this matrix]
    (update this :control-curves #(map (fn [cc] (protocols/linear-transform cc matrix)) %))))

(defn tensor-product-patch
  "Creates new tensor product patch"
  [slice-fn control-curves]
  (map->TensorProductPatch {:slice-fn       slice-fn
                            :control-curves control-curves}))

(defn bezier-patch
  "Creates new bezier patch"
  [control-curves]
  (tensor-product-patch bezier-curve control-curves))

(defrecord CompositeBezierPatch [control-curve-sequences]
  protocols/PParametricPatch
  (slice-points [_ t slice-points-count]
    (protocols/points
     (composite-bezier-curve (map (fn [control-curves]
                                    (map #(protocols/point % t)
                                         control-curves))
                                  control-curve-sequences))
     slice-points-count))
  (patch-points [this slice-points-count slices-count]
    (resolve-curve slices-count #(protocols/slice-points this % slice-points-count)))
  protocols/PTransformable
  (linear-transform [this matrix]
    (update this :control-curve-sequences
            #(map (fn [control-curves]
                    (map (fn [cc] (protocols/linear-transform cc matrix))
                         control-curves)) %))))

(defn composite-bezier-patch
  "Creates new composite bezier patch"
  [control-curve-sequences]
  (->CompositeBezierPatch control-curve-sequences))

(defrecord BSplinePatch [control-curves knot-vector order]
  protocols/PParametricPatch
  (slice-points [_ t slice-points-count]
    (protocols/points
     (b-spline {:control-points (map #(protocols/point % t) control-curves)
                :knot-vector    knot-vector
                :order          order})
     slice-points-count))
  (patch-points [this slice-points-count slices-count]
    (resolve-curve slices-count #(protocols/slice-points this % slice-points-count)))
  protocols/PTransformable
  (linear-transform [this matrix]
    (update this :control-curves #(map (fn [cc] (protocols/linear-transform cc matrix)) %))))

(defn b-spline-patch
  "Creates new b-spline patch"
  [opts]
  (map->BSplinePatch opts))

(defn clamped-b-spline-patch
  "Creates new clamped b-spline-patch"
  [opts]
  (b-spline-patch (clamp-knot-vector opts)))

(comment
  (require '[chisel.open-scad :as os])
  (require '[chisel.stl :as stl])
  ;; Fast surf-ski hull planform
  (os/write
   (os/generate-polygon
    (protocols/points
     (composite-bezier-curve [[(c/v [-80 0]) (c/v [-80 2]) (c/v [-10 5]) (c/v [0 5])]
                              [(c/v [0 5]) (c/v [25 5]) (c/v [70 2]) (c/v [70 0])]
                              [(c/v [70 0]) (c/v [70 -2]) (c/v [25 -5]) (c/v [0 -5])]
                              [(c/v [0 -5]) (c/v [-10 -5]) (c/v [-80 -2]) (c/v [-80 0])]])
     1000)))
  ;; Inverted bow yacht hull
  (os/write
   (os/generate-polyhedron
    (os/extrude-between-polygons
     (protocols/patch-points
      (bezier-patch
       [(bezier-curve [(c/v [5 0 0]) (c/v [-5 0 50]) (c/v [1 0 100])])
        (bezier-curve [(c/v [5 10 -5]) (c/v [-5 12 50]) (c/v [1 5 95])])
        (bezier-curve [(c/v [5 10 -5]) (c/v [15 12 50]) (c/v [9 5 95])])
        (bezier-curve [(c/v [5 0 0]) (c/v [15 0 50]) (c/v [9 0 100])])])
      100 100))))
  ;; Fast surf-ski hull
  (os/write
   (os/generate-polyhedron
    (os/extrude-between-polygons
     (protocols/patch-points
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
      100 400))))
  ;; Fast surf-ski hull take 2
  (os/write
   (os/generate-polyhedron
    (os/extrude-between-polygons
     (protocols/patch-points
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
      100 400))))

  (stl/write
   (stl/generate-ascii-solid
    (protocols/patch-points
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
      100 400))))
