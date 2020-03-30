(ns chisel.curves
  "Various curve implementations"
  (:require [chisel.protocols :as protocols]
            [chisel.vectors :as vectors]))

(defn de-casteljau
  "De Casteljau algorithm for recursively calculating Bezier curve point 
  from parameter `t` and sequence of `control-points`"
  [t [point-1 point-2 :as control-points]]
  (if (= 2 (count control-points))
    (vectors/add (vectors/scalar-multiply point-1 (- 1 t))
                 (vectors/scalar-multiply point-2 t))
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
    (keep (comp curve-fn #(/ % denominator))
          (range (if drop-first? 1 0)
                 (if drop-first? (inc points-count) points-count)))))

(defn- parameter-assertion [t]
  (assert (>= 1 t 0) "parameter `t` has to be in (inclusive) range [0 1]"))

(defrecord BezierCurve [control-points]
  protocols/PParametricCurve
  (point [_ t]
    (parameter-assertion t)
    (de-casteljau t control-points))
  (points [_ points-count]
    (resolve-curve points-count #(de-casteljau % control-points))))

(defn- control-points-assertions [control-points]
  (assert (> (count control-points) 1)
          "at least two `control-points` are required for Bezier curve")
  (assert (every? (partial every? number?)
                  control-points)
          "`control-points` needs to be sequence of number tuples"))

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
    (de-casteljau local-t control-points)))

(defrecord CompositeBezierCurve [control-points-sequence]
  protocols/PParametricCurve
  (point [_ t]
    (parameter-assertion t)
    (resolve-composite-curve-point t control-points-sequence))
  (points [_ points-count]
    (resolve-curve points-count #(resolve-composite-curve-point % control-points-sequence))))

(defn- control-points-sequence-assertions [control-points-sequence]
  (doall (map control-points-assertions control-points-sequence))
  (assert (= (rest (map first control-points-sequence))
             (butlast (map last control-points-sequence)))
          "control points in the `control-points-sequence` need to be connected"))

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
  (if (= 2 (count span-cp-tuples))
    (let [coefficient (/ (- t from) (- to from))]
      [[from to]
       (vectors/add (vectors/scalar-multiply point-1 (- 1 coefficient))
                    (vectors/scalar-multiply point-2 coefficient))])
    (de-boor t (map (partial de-boor t) (partition 2 1 span-cp-tuples)))))

(defn- resolve-bspline-point [t span-cp-tuples minimal-cps]
  (let [effective-span-cp-tuples (parameter->effective-curve t span-cp-tuples)]
    (when (>= (count effective-span-cp-tuples) minimal-cps)
      (second (de-boor t effective-span-cp-tuples)))))

(defrecord BSplineCurve [span-cp-tuples minimal-cps]
  protocols/PParametricCurve
  (point [_ t]
    (parameter-assertion t)
    (resolve-bspline-point t span-cp-tuples minimal-cps))
  (points [_ points-count]
    (resolve-curve points-count #(resolve-bspline-point % span-cp-tuples minimal-cps))))

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
   :minimal-cps    (inc order)})

(defn b-spline
  "Creates new b-spline curve"
  [opts]
  (b-spline-assertions opts)
  (map->BSplineCurve (process-b-spline opts)))

(defrecord BezierPatch [control-curves]
  protocols/PParametricPatch
  (slice-points [_ t slice-points-count]
    (protocols/points
     (bezier-curve (map #(protocols/point % t)
                        control-curves))
     slice-points-count))
  (patch-points [this slice-points-count slices-count]
    (resolve-curve slices-count #(protocols/slice-points this % slice-points-count))))

(defn bezier-patch
  "Creates new bezier patch"
  [control-curves]
  (->BezierPatch control-curves))

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
    (resolve-curve slices-count #(protocols/slice-points this % slice-points-count))))

(defn composite-bezier-patch
  "Creates new composite bezier patch"
  [control-curve-sequences]
  (->CompositeBezierPatch control-curve-sequences))

(comment
  (require '[chisel.open-scad :as os])
  ;; Fast surf-ski hull planform
  (os/write
   (os/generate-polygon
    (protocols/points
     (composite-bezier-curve [[[-80 0] [-80 2] [-10 5] [0 5]]
                             [[0 5] [25 5] [70 2] [70 0]]
                             [[70 0] [70 -2] [25 -5] [0 -5]]
                             [[0 -5] [-10 -5] [-80 -2] [-80 0]]])
     1000)))
  ;; Inverted bow yacht hull
  (os/write
   (os/generate-polyhedron
    (os/extrude-between-polygons
     (protocols/patch-points
      (bezier-patch
       [(bezier-curve [[5 0 0] [-5 0 50] [1 0 100]])
        (bezier-curve [[5 10 -5] [-5 12 50] [1 5 95]])
        (bezier-curve [[5 10 -5] [15 12 50] [9 5 95]])
        (bezier-curve [[5 0 0] [15 0 50] [9 0 100]])])
      100 100))))
  ;; Fast surf-ski hull
  (os/write
   (os/generate-polyhedron
    (os/extrude-between-polygons
     (protocols/patch-points
      (bezier-patch
       [(composite-bezier-curve
         [[[-80 0 0] [-80 2 0] [-10 5 0] [0 5 0]]
          [[0 5 0] [25 5 0] [70 2 0] [70 0 0]]])
        (composite-bezier-curve
         [[[-80 0 0] [-105 1 5] [-80 1 5]]
          [[-80 1 5] [-10 5 5] [0 5 5]]
          [[0 5 5] [25 5 5] [70 2 5] [70 0 5]]])
        (composite-bezier-curve
         [[[-80 0 0] [-105 -1 5] [-80 -1 5]]
          [[-80 -1 5] [-10 -5 5] [0 -5 5]]
          [[0 -5 5] [25 -5 5] [70 -2 5] [70 0 5]]])
        (composite-bezier-curve
         [[[-80 0 0] [-80 -2 0] [-10 -5 0] [0 -5 0]]
          [[0 -5 0] [25 -5 0] [70 -2 0] [70 0 0]]])])
      100 400)))))
