(ns chisel.infills
  "Infill implementations"
  (:require [chisel.protocols :as protocols]
            [chisel.curves :as curves]
            [chisel.coordinates :as c]
            [chisel.utils :as u]))
(comment
  (def ^:const full-circle (* 2 Math/PI))

  (def ^:const right-angle (/ Math/PI 2))

  (def ^:const eqilateral-height (/ (Math/sqrt 3) 2))

  (defn- right-angles [start angles-count]
    (map #(rem (* % right-angle) full-circle) (range start (+ start angles-count))))

  (defn connector
    [from to origin d i j])

  (defn triangle-infill
    "Triangle infill polyhedron"
    [i-triangles j-triangles thickness-ratio inner-patch outer-patch]
    (assert (even? j-triangles) "Triangle count has to be even for j-dimension")
    (assert (< thickness-ratio i-triangles j-triangles) "Thickness ratio has to be less then triangle divisio?n")
    ))

;; Grid infill

(defn- grid-cell-ratio [thickness]
  (/ 1 (+ thickness (* thickness (- 1 thickness)))))

(defn- rectangular-coordinates [origin side-a side-b]
  (reductions (partial mapv +) origin [[side-a 0] [0 (- side-b)] [(- side-a) 0]]))

(defn- top-curve [[c0 c1]]
  (curves/bezier-curve [(c/v c0) (c/v c1)]))

(defn- bottom-curve [[_ _ c2 c3]]
  (curves/bezier-curve [(c/v c3) (c/v c2)]))

(defn- right-curve [[_ c1 c2]]
  (curves/bezier-curve [(c/v c1) (c/v c2)]))

(defn- left-curve [[c0 _ _ c3]]
  (curves/bezier-curve [(c/v c0) (c/v c3)]))

(defn- grid-corners [[i j] spacing-a thickness-a spacing-b thickness-b]
  (map #(rectangular-coordinates % thickness-a thickness-b)
       (rectangular-coordinates [i j] (- spacing-a thickness-a) (- spacing-b thickness-b))))

(defn- accumulate-grid [spacing-a thickness-a spacing-b thickness-b acc origin]
  (let [[upper-left upper-right lower-right lower-left]
        (grid-corners origin spacing-a thickness-a spacing-b thickness-b)]
    (-> acc
        (protocols/stitch (curves/bezier-patch [(right-curve upper-left) (left-curve upper-right)]))
        (protocols/stitch (curves/bezier-patch [(top-curve lower-right) (bottom-curve upper-right)]))
        (protocols/stitch (curves/bezier-patch [(right-curve lower-left) (left-curve lower-right)]))
        (protocols/stitch (curves/bezier-patch [(top-curve lower-left) (bottom-curve upper-left)]))
        (protocols/stitch (curves/bezier-patch [(left-curve upper-right) (right-curve upper-right)]))
        (protocols/stitch (curves/bezier-patch [(left-curve lower-right) (right-curve lower-right)]))
        (protocols/stitch (curves/bezier-patch [(left-curve lower-left) (right-curve lower-left)]))
        (protocols/stitch (curves/bezier-patch [(left-curve upper-left) (right-curve upper-left)])))))

(defn grid-infill
  ([rectangles thickness-ratio]
   (grid-infill rectangles rectangles thickness-ratio))
  ([rectangles-a rectangles-b thickness-ratio]
   (let [step-a           (/ 1 rectangles-a)
         half-thickness-a (* step-a (/ thickness-ratio 2))
         step-b           (/ 1 rectangles-b)
         half-thickness-b (* step-b (/ thickness-ratio 2))]
     (reduce (partial accumulate-grid step-a half-thickness-a step-b half-thickness-b)
             (curves/stitched-patch)
             (for [x (range 0 1 step-a) y (range 1 0 (- step-b))] [x y])))))

;; Triangle infill

(defn- accumulate-triangles [spacing-a thickness-a spacing-b thickness-b  acc [origin down?]]
  (let [[upper-left upper-right lower-right lower-left]
        (grid-corners origin spacing-a thickness-a spacing-b thickness-b)]
    (if down?
      (-> acc
          (protocols/stitch (curves/bezier-patch [(right-curve upper-left) (left-curve lower-right)]))
          (protocols/stitch (curves/bezier-patch [(top-curve lower-right) (top-curve upper-right)]))
          (protocols/stitch (curves/bezier-patch [(left-curve lower-right) (right-curve lower-right)]))
          (protocols/stitch (curves/bezier-patch [(bottom-curve lower-left) (bottom-curve upper-left)]))
          (protocols/stitch (curves/bezier-patch [(left-curve upper-left) (right-curve upper-left)])))
      (-> acc
          (protocols/stitch (curves/bezier-patch [(right-curve lower-left) (left-curve upper-right)]))
          (protocols/stitch (curves/bezier-patch [(top-curve lower-left) (top-curve upper-left)]))
          (protocols/stitch (curves/bezier-patch [(left-curve lower-left) (right-curve lower-left)]))
          (protocols/stitch (curves/bezier-patch [(bottom-curve lower-right) (bottom-curve upper-right)]))
          (protocols/stitch (curves/bezier-patch [(left-curve upper-right) (right-curve upper-right)]))))))

(defn- triangle-thickness [spacing-a spacing-b thickness-a]
  (* 2 thickness-a (/ (u/hypotenuse spacing-a spacing-b) spacing-a)))

(defn- add-base [patch triangles step offset base]
  (reduce (fn [acc origin]
            (let [left  (rectangular-coordinates origin offset base)
                  right (rectangular-coordinates (update origin 0 + (- step offset)) offset base)]
              (-> acc
                  (protocols/stitch (curves/bezier-patch [(right-curve left) (left-curve right)]))
                  (protocols/stitch (curves/bezier-patch [(left-curve left) (right-curve left)]))
                  (protocols/stitch (curves/bezier-patch [(left-curve right) (right-curve right)])))))
          patch
          (for [x (range 0 triangles)]
            (let [origin-x (* x step)]
              [origin-x 1]))))

(defn triangle-infill
  ([triangles thickness-ratio]
   (triangle-infill triangles triangles thickness-ratio :base thickness-ratio))
  ([triangles-a triangles-b thickness-ratio & {:keys [base] :or {base 0}}]
   (let [base             (/ base triangles-a)
         step-a           (/ 1 triangles-a)
         half-thickness-a (* step-a (/ thickness-ratio 2))
         step-b           (/ (- 1 base) triangles-b)
         half-thickness-b (triangle-thickness triangles-b triangles-a half-thickness-a)
         result           (reduce (partial accumulate-triangles step-a half-thickness-a step-b half-thickness-b)
                                  (curves/stitched-patch)
                                  (for [x (range 0 triangles-a)
                                        y (range triangles-b 0 -1)]
                                    (let [origin-x (* x step-a)
                                          origin-y (* y step-b)]
                                      [[origin-x origin-y] (even? (+ x y))])))]
     (cond-> result
       (not (zero? base)) (add-base triangles-a step-a half-thickness-a base)))))

;; Generic infill functionality

(defn- map-infill-mesh [patch mesh]
  (update mesh :points #(mapv (fn [v] (protocols/patch-point patch (v 0) (v 1))) %)))

(defn- map-polyline [patch polyline]
  (mapv (fn [v] (protocols/patch-point patch (v 0) (v 1))) polyline))

(defn- polyline-resolution [i j direction]
  (get {:i i :j j} direction))

(defn infill-polyhedron
  [infill-patch lower-patch upper-patch [i j]]
  (let [perimeter-curves (reduce (fn [acc [[c-1 c-2] dir]]
                                   (cond-> (assoc acc c-1 dir)
                                     c-2 (assoc c-2 dir)))
                                 {}
                                 (protocols/perimeter-curves infill-patch))
        infill-mesh      (protocols/triangle-mesh infill-patch [i j])]
    (reduce-kv (fn [acc curve direction]
                 (let [points-count    (polyline-resolution i j direction)
                       polyline        (protocols/polyline curve points-count)
                       faces           (u/triangulate-rectangular-mesh 2 points-count)
                       connecting-mesh {:points (into (map-polyline lower-patch polyline)
                                                      (map-polyline upper-patch polyline))
                                        :faces  faces}]
                   (u/merge-triangle-meshes acc connecting-mesh)))
               (u/merge-triangle-meshes
                (u/reverse-polyhedron-faces
                 (map-infill-mesh lower-patch infill-mesh))
                (map-infill-mesh upper-patch infill-mesh))
               perimeter-curves)))

(comment
  (def p1
    (curves/bezier-patch
     [(curves/clamped-b-spline
       {:control-points [(c/v [-60 0 30]) (c/v [-60 0 0]) (c/v [0 0 0]) (c/v [60 0 0]) (c/v [60 0 30])]
        :knot-vector    [1/3 2/3]
        :order          2})
      (curves/clamped-b-spline
       {:control-points [(c/v [-60 60 30]) (c/v [-60 60 0]) (c/v [0 60 0]) (c/v [60 60 0]) (c/v [60 60 30])]
        :knot-vector    [1/3 2/3]
        :order          2})]))

  (def p2
    (curves/bezier-patch
     [(curves/clamped-b-spline
       {:control-points [(c/v [-50 0 30]) (c/v [-50 0 10]) (c/v [0 0 10]) (c/v [50 0 10]) (c/v [50 0 30])]
        :knot-vector    [1/3 2/3]
        :order          2})
      (curves/clamped-b-spline
       {:control-points [(c/v [-50 60 30]) (c/v [-50 60 10]) (c/v [0 60 10]) (c/v [50 60 10]) (c/v [50 60 30])]
        :knot-vector    [1/3 2/3]
        :order          2})]))

  (def g (triangle-infill 10 6 1/10 :base 1/10))

  (def infill-ph
    (infill-polyhedron g p1 p2 [4 10]))

  (stl/write
   (stl/generate-ascii-solid infill-ph)))
