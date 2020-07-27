(ns chisel.utils
  "Utility fns")

(defn merge-triangle-meshes
  "Merge 2 triangle meshes together"
  [mesh-1 {:keys [points faces]}]
  (let [offset (count (:points mesh-1))]
    (-> mesh-1
        (update :points into points)
        (update :faces into (map (fn [[v1 v2 v3]]
                                   [(+ v1 offset) (+ v2 offset) (+ v3 offset)])) faces))))
(defn triangulate-rectangular-mesh
  "Returns set of faces for [i j] rectangular mesh"
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

(defn reverse-polyhedron-faces
  "Reverse direction of polyhedron faces"
  [polyhedron]
  (update polyhedron :faces #(mapv (fn [[v0 v1 v2]] [v2 v1 v0]) %)))

(defn hypotenuse
  "Given 2 shorter sides of right-angle triangle, return the length of the longest side"
  [a b]
  (rationalize (Math/sqrt (+ (Math/pow a 2) (Math/pow b 2)))))
