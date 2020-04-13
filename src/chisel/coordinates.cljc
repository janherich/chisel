(ns chisel.coordinates
  "Coordinate/Vector operations"
  (:require [chisel.protocols :as protocols]))

(extend-protocol protocols/PHomogenousCoordinate
  clojure.lang.PersistentVector
  (coordinates [this] this)
  (weight [this] 1)
  (project [this] this))

(defrecord HomogenousCoordinate [coordinates weight]
  protocols/PHomogenousCoordinate
  (coordinates [_] coordinates)
  (weight [_] weight)
  (project [_]
    (mapv #(/ % weight) coordinates)))

(defn add-coordinates
  "Add coordinates"
  [coordinate-1 coordinate-2]
  (HomogenousCoordinate.
   (mapv + (protocols/coordinates coordinate-1) (protocols/coordinates coordinate-2))
   (+ (protocols/weight coordinate-1) (protocols/weight coordinate-2))))

(defn diff-coordinates
  "Diff coordinates"
  [coordinate-1 coordinate-2]
  (HomogenousCoordinate.
   (mapv - (protocols/coordinates coordinate-1) (protocols/coordinates coordinate-2))
   (+ (protocols/weight coordinate-1) (protocols/weight coordinate-2))))

(defn scalar-multiply-coordinates
  "Scalar multiply coordinates"
  [coordinate scalar]
  (HomogenousCoordinate.
   (mapv (partial * scalar) (protocols/coordinates coordinate))
   (* (protocols/weight coordinate) scalar)))

(defn project-coordinate
  "Project coordinate to euclidian plane"
  [coordinate]
  (protocols/project coordinate))

(defn euclidian->homogenous
  "Converts euclidian coordinate to equivalent homogenous coordinate"
  [coordinate weight]
  (scalar-multiply-coordinates coordinate weight))
