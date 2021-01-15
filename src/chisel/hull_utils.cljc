(ns chisel.hull-utils
  "Hull utility fns"
  (:require [chisel.protocols :as protocols]
            [chisel.coordinates :as c]))

(defn polygon-area
  "Calculates area of flat polygon which should be defined in X/Y plane"
  [polyline-vector]
  (let [polyline-count (count polyline-vector)]
    (-> (reduce (fn [acc idx]
                  (let [v1 (get polyline-vector idx)
                        v2 (get polyline-vector (mod (inc idx) polyline-count))]
                    (+ acc (- (* (v1 0) (v2 1))
                              (* (v2 0) (v1 1))))))
                0
                (range polyline-count))
        (/ 2)
        double
        Math/abs)))

(defn- in-range? [a b c]
  (>= (Math/max a c) b (Math/min a c)))

(defn- between-ratio [y1 y2 y-cutoff]
  (/ (- y2 y-cutoff) (- y2 y1)))

(defn cut-polygon
  "Cuts polygon define in X/Y plane along Y axis in negative direction"
  [polyline-vector y-cutoff]
  (reduce (fn [acc [p1 p2]]
            (let [y1 (p1 1)
                  y2 (p2 1)]
              (cond-> acc
                (> y-cutoff y1)
                (conj p1)

                (in-range? y1 y-cutoff y2)
                (conj (c/linear-combination (between-ratio y1 y2 y-cutoff) p2 p1)))))
          []
          (partition 2 1 (conj polyline-vector (first polyline-vector)))))

(defn prismatoid-volume
  "Given 2 parallel polygons oriented in X/Y plane, calculates volume of resulting prismatoid"
  [slice-1 slice-2 slice-resolution y-cut]
  (let [polyline-1      (protocols/polyline slice-1 slice-resolution)
        polyline-2      (protocols/polyline slice-2 slice-resolution)
        spacing         (- ((first polyline-1) 2) ((first polyline-2) 2))
        middle-polyline (into []
                              (mapcat (fn [idx]
                                        (let [p-1-point      (get polyline-1 idx)
                                              p-2-point      (get polyline-2 idx)
                                              p-2-next-point (get polyline-2
                                                                  (mod (inc idx) slice-resolution))]
                                          [(c/linear-combination 1/2 p-1-point p-2-point)
                                           (c/linear-combination 1/2 p-1-point p-2-next-point)])))
                              (range slice-resolution))]
    (Math/abs (* (/ spacing 6)
                 (+ (polygon-area (cond-> polyline-1 y-cut (cut-polygon y-cut)))
                    (* 4 (polygon-area (cond-> middle-polyline y-cut (cut-polygon y-cut))))
                    (polygon-area (cond-> polyline-2 y-cut (cut-polygon y-cut))))))))

(defn hull-volume
  "Calculates volume of hull composed of parallel flat slices in the X/Y plane,
  optionally cut in the y-direction"
  ([hull-patch hull-slices-count slice-resolution]
   (hull-volume hull-patch hull-slices-count slice-resolution nil))
  ([hull-patch hull-slices-count slice-resolution y-cut]
   (let [hull-step   (/ 1 hull-slices-count)
         hull-slices (map #(protocols/patch-slice hull-patch %)
                          (range 0 (+ 1 hull-step) hull-step))]
     (reduce (fn [acc [slice-1 slice-2]]
               (+ acc (prismatoid-volume slice-1 slice-2 slice-resolution y-cut)))
             0
             (partition 2 1 hull-slices)))))
