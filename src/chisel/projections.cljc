(ns chisel.projections
  "Projections of euclidian points"
  (:require [chisel.protocols :as protocols]
            [chisel.coordinates :as c]))

(defn to-cylinder
  "Projects euclidian point in orthogonal to cylindrical space"
  [[x y z]]
  (let [alpha (/ y z)]
    [x
     (* z (Math/sin alpha))
     (* z (Math/cos alpha))]))

(defn rotate
  [angle [x y z]]
  [x
   (- (* y (Math/cos angle)) (* z (Math/sin angle)))
   (+ (* y (Math/sin angle)) (* z (Math/cos angle)))])
