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

;; Homogenous coordinate operations
(defn add-coordinates
  "Add homogenous coordinates"
  ([c1 c2]
   (HomogenousCoordinate.
    (mapv + (protocols/coordinates c1) (protocols/coordinates c2))
    (+ (protocols/weight c1) (protocols/weight c2))))
  ([c1 c2 & coordinates]
   (HomogenousCoordinate.
    (reduce (partial mapv +) (map protocols/coordinates (conj coordinates c1 c2)))
    (reduce + (map protocols/weight (conj coordinates c1 c2))))))

(defn scalar-multiply-coordinates
  "Scalar multiply homogenous coordinates"
  [coordinate scalar]
  (HomogenousCoordinate.
   (mapv (partial * scalar) (protocols/coordinates coordinate))
   (* (protocols/weight coordinate) scalar)))

(defn project-coordinate
  "Project homogenous coordinate to euclidian plane"
  [coordinate]
  (protocols/project coordinate))

(defn euclidian->homogenous
  "Converts euclidian coordinate to equivalent homogenous coordinate"
  [coordinate weight]
  (scalar-multiply-coordinates coordinate weight))

;; Vector (plain euclidian coordinates) operations
(defn add-vectors
  "Vector addition"
  ([v1 v2]
   (mapv + v1 v2))
  ([v1 v2 & vectors]
   (apply mapv + v1 v2 vectors)))

(defn diff-vectors
  "Vector difference"
  [v1 v2]
  (mapv - v1 v2))

(defn opposite-vector
  "Creates vector of opposite direction to the original one"
  [v]
  (mapv unchecked-negate v))
