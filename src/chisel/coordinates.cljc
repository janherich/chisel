(ns chisel.coordinates
  "Coordinate/Vector operations"
  (:require [chisel.protocols :as protocols]
            [uncomplicate.neanderthal.core :as matrix]
            [uncomplicate.neanderthal.native :as native]
            [uncomplicate.neanderthal.vect-math :as vect-math]))

(defn sum
  ([v1 v2]
   (matrix/axpy v1 v2))
  ([v1 v2 & vectors]
   (apply matrix/axpy v1 v2 vectors)))

(defn difference
  ([v1 v2]
   (matrix/axpy v1 (matrix/ax -1 v2)))
  ([v1 v2 & vectors]
   (apply matrix/axpy (matrix/ax -1 v2) (map (partial matrix/ax -1) vectors))))

(defn multiply [c v]
  (matrix/ax c v))

(defn translate-matrix [[x y z]]
  (native/dge 4 4 [1 0 0 0
                   0 1 0 0
                   0 0 1 0
                   x y z 1]))

(defn scale-matrix [scalar]
  (native/dge 4 4 [scalar 0 0 0
                   0 scalar 0 0
                   0 0 scalar 0
                   0 0 0 1]))

(defn linear-combination
  [t c1 c2]
  (matrix/axpy (matrix/ax (- 1 t) c1) (matrix/ax t c2)))

(defn vector-length
  [v]
  (matrix/nrm2 (native/dv (v 0) (v 1) (v 2))))

(defn scale-vector
  [v desired-length]
  (let [v-length (matrix/nrm2 v)
        ratio    (if (zero? v-length) 0 (/ desired-length v-length))]
    (matrix/mv (scale-matrix ratio) v)))

(defn project [coordinate]
  (matrix/ax (/ 1 (coordinate 3)) coordinate))

(defn v [c]
  (apply native/dv (concat c (drop (count c) [0 0 0 1]))))

(defn weighted [coordinate weight]
  (matrix/ax weight coordinate))

(defn opposite-vector [v]
  (matrix/mv (native/dge 4 4 [-1 0 0 0
                              0 -1 0 0
                              0 0 -1 0
                              0 0 0 1])
             v))

(defn orthogonal-vector [v & {:keys [counterclockwise?]}]
  (matrix/mv (if counterclockwise?
               (native/dge 4 4 [0 1 0 0
                                -1 0 0 0
                                0 0 1 0
                                0 0 0 1])
               (native/dge 4 4 [0 -1 0 0
                                1 0 0 0
                                0 0 1 0
                                0 0 0 1]))
             v))

(defn valid-point? [v]
  (= 4 (matrix/dim v)))

(extend-protocol protocols/PTransformable
  uncomplicate.neanderthal.internal.host.buffer_block.RealBlockVector
  (linear-transform [this matrix]
    (matrix/mv matrix this)))
