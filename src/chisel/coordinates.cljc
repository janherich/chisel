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

(defn combine-matrices
  ([c1 c2]
   (matrix/mm c1 c2))
  ([c1 c2 & matrices]
   (apply matrix/mm c1 c2 matrices)))

(def ^:private axis->idx {:x 0 :y 5 :z 10})

(def ^:private identity-matrix
  [1 0 0 0
   0 1 0 0
   0 0 1 0
   0 0 0 1])

(defn translate-matrix [v]
  (let [x (v 0)
        y (v 1)
        z (v 2)]
    (native/dge 4 4 [1 0 0 0
                     0 1 0 0
                     0 0 1 0
                     x y z 1])))

(defn scale-matrix [scale-opts]
  (let [axis->scale (if (map? scale-opts)
                      scale-opts
                      {:x scale-opts
                       :y scale-opts
                       :z scale-opts})]
    (native/dge
     4 4 (reduce-kv (fn [acc axis scale]
                      (assoc acc (axis->idx axis) scale))
                    identity-matrix
                    axis->scale))))

(defn flip-matrix [axis]
  (assert (#{:x :y :z} axis) "Flip axis must be either `:x`, `:y` or `:z`")
  (scale-matrix {axis -1}))

(defn rotate-matrix
  "Rotates point in x/y plane"
  [angle]
  (let [cos-angle (Math/cos angle)
        sin-angle (Math/sin angle)]
    (native/dge 4 4 [cos-angle (- sin-angle) 0 0
                     sin-angle cos-angle     0 0
                     0         0             1 0
                     0         0             0 1])))

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
