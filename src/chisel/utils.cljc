(ns chisel.utils
  "Utility fns"
  (:require [chisel.coordinates :as c]
            [chisel.protocols :as protocols]
            [clojure.string :as string]
            [clojure.java.io :as io]))

;; Utility fn to write to file
(defn write-to-file [path & content]
  (with-open [w (io/writer path)]
    (.write w (string/join "\n" content))))

(defn relative-parameter-assertion
  "Asserts relative parameter value"
  [t]
  (assert (>= 1 t 0)
          (format "parameter `t` has to be in (inclusive) range [0 1], :t = %s" t)))

(defn merge-triangle-meshes
  "Merge 2 or more triangle meshes together"
  ([mesh-1 {:keys [points faces]}]
   (let [offset (count (:points mesh-1))]
     (-> mesh-1
         (update :points into points)
         (update :faces into (map (fn [[v1 v2 v3]]
                                    [(+ v1 offset) (+ v2 offset) (+ v3 offset)])) faces))))
  ([mesh-1 mesh-2 & meshes]
   (reduce merge-triangle-meshes mesh-1 (cons mesh-2 meshes))))

(defn triangulate-rectangular-mesh
  "Returns vector of faces for [i j] rectangular mesh"
  [i-count j-count]
  (transduce (map (fn [i]
                    (let [offset (* i j-count)]
                      (into []
                            (mapcat (fn [j]
                                      (let [a (+ j offset)
                                            b (inc a)
                                            c (+ b j-count)
                                            d (+ a j-count)]
                                        [[a b d] [d b c]])))
                            (range (dec j-count))))))
             into
             (range (dec i-count))))

(defn triangulate-triangle-mesh
  "Returns vector of faces for triangular mesh"
  [side-points]
  (transduce (map (fn [i]
                    (let [offset  (/ (* i (inc i)) 2)
                          final-a (+ i offset)
                          final-c (+ final-a (inc i))
                          final-d (inc final-c)]
                      (conj (into []
                                  (mapcat (fn [j]
                                            (let [a (+ j offset)
                                                  b (inc a)
                                                  c (+ b i)
                                                  d (inc c)]
                                              [[a d c] [a b d]])))
                                  (range i))
                            [final-a final-d final-c]))))
             into
             (range (dec side-points))))

(defn right-angle-triangle-points
  "Returns 2d right angle triangle points"
  ([i j resolution]
   (right-angle-triangle-points [0 0] i j resolution))
  ([[origin-x origin-y] i j resolution]
   (let [i-step (/ i (dec resolution))
         j-step (/ j (dec resolution))]
     (transduce (map (fn [i]
                       (map (fn [j]
                              [(+ origin-x (* i i-step))
                               (+ origin-y (* j j-step))])
                            (range (inc i)))))
                into
                (range resolution)))))

(defn reverse-polyhedron-faces
  "Reverse direction of polyhedron faces"
  [polyhedron]
  (update polyhedron :faces #(mapv (fn [[v0 v1 v2]] [v2 v1 v0]) %)))

(defn hypotenuse
  "Given 2 shorter sides of right-angle triangle, return the length of the longest side"
  [a b]
  (rationalize (Math/sqrt (+ (Math/pow a 2) (Math/pow b 2)))))

(defn cross-product
  "Calculates cross product of two 3d vectors"
  [v1 v2]
  (let [x1 (v1 0)
        y1 (v1 1)
        z1 (v1 2)
        x2 (v2 0)
        y2 (v2 1)
        z2 (v2 2)]
    (c/v [(- (* y1 z2) (* z1 y2))
          (- (* z1 x2) (* x1 z2))
          (- (* x1 y2) (* y1 x2))])))

(defn triangle-mesh-surface
  "Calculates surface of the triangle mesh"
  [{:keys [points faces]}]
  (Math/abs
   (/ (reduce (fn [surface [v1 v2 v3]]
                (let [p1     (points v1)
                      p2     (points v2)
                      p3     (points v3)
                      p1->p2 (c/difference p2 p1)
                      p1->p3 (c/difference p3 p1)]
                  (+ surface (c/vector-length (cross-product p1->p2 p1->p3)))))
              0 faces)
      2)))

(defn triangle-mesh-volume
  "Calculates volume of the triangle mesh by signed tetrahedron volume method"
  [{:keys [points faces]}]
  (Math/abs
   (reduce (fn [volume [v1 v2 v3]]
             (let [p1   (points v1)
                   p2   (points v2)
                   p3   (points v3)
                   v321 (* (p3 0) (p2 1) (p1 2))
                   v231 (* (p2 0) (p3 1) (p1 2))
                   v312 (* (p3 0) (p1 1) (p2 2))
                   v132 (* (p1 0) (p3 1) (p2 2))
                   v213 (* (p2 0) (p1 1) (p3 2))
                   v123 (* (p1 0) (p2 1) (p3 2))]
               (+ volume (* 1/6 (- (+ v231 v312 v123) v321 v132 v213)))))
           0 faces)))

(defn line-length
  "Length of the line"
  [[start end]]
  (c/vector-length (c/difference end start)))

(defn polyline-length
  "Calculates length of the given polyline"
  [polyline]
  (transduce (map line-length) + (partition 2 1 polyline)))

(defn make-range-tree
  "Constructs range-tree structure out of sorted vector of maps, each of them expecting to contain
  at least `:range` and `:interpolate-fn` keys.
  Range of each item should be in `[lower upper]` interval, interpolate fn is function taking at 
  least one argument `t` belonging to the interval range."
  [ranges-vector]
  (let [ranges-count      (count ranges-vector)
        median-index      (int (/ ranges-count 2))
        median-plus-index (inc median-index)]
    (cond-> (get ranges-vector median-index)
      (< 0 median-index)
      (assoc :lower-range (make-range-tree (subvec ranges-vector 0 median-index)))
      (< median-plus-index ranges-count)
      (assoc :upper-range (make-range-tree (subvec ranges-vector median-plus-index ranges-count))))))

(defn search-range-tree
  "Given range-tree structure constructed by `make-range-tree` fn, parameter `t` and any number
  of following arguments, this function will effeciently search for the record which has `t`
  in (inclusive) range and call the record `interpolate` fn with `t` and any number of following
  arguments."
  [{:keys [range lower-range upper-range interpolate-fn]} t & args]
  (let [[start end] range]
    (cond
      (< t start) (apply search-range-tree lower-range t args)
      (> t end)   (apply search-range-tree upper-range t args)
      :else       (apply interpolate-fn t args))))

(defn prepare-polyline [polyline]
  (let [first-point (first polyline)
        last-point  (peek polyline)]
    (let [second-point  (second polyline)
          butlast-point (peek (pop polyline))]
      (if (= first-point last-point)
        (into [butlast-point] (conj polyline second-point))
        (let [prepend-point (c/sum first-point (c/difference first-point second-point))
              append-point  (c/sum last-point (c/difference last-point butlast-point))]
          (into [prepend-point] (conj polyline append-point)))))))

(defn distanced-path
  "Returns polyline distance by perpendicular distance from original polyline vector"
  [polyline distance]
  (mapv (fn [[a b c]]
          (let [a-orthogonal   (c/orthogonal-vector
                                (c/scale-vector (c/difference a b) distance))
                c-orthogonal   (c/orthogonal-vector
                                (c/scale-vector (c/difference b c) distance))
                orthogonal-sum (c/sum a-orthogonal c-orthogonal)
                b-distanced    (c/sum
                                b
                                (if (= a-orthogonal c-orthogonal)
                                  a-orthogonal
                                  (c/scale-vector orthogonal-sum
                                                  (Math/abs (/ distance
                                                               (Math/cos
                                                                (Math/atan (/ (c/vector-length
                                                                               (c/difference a-orthogonal
                                                                                             c-orthogonal))
                                                                              (c/vector-length orthogonal-sum)))))))))]
            b-distanced))
          (partition 3 1 (prepare-polyline polyline))))

