(ns chisel.vectors
  "Vector operations")

(defn add
  "Vector sum"
  [vector-1 & vectors]
  (reduce (partial mapv +) vector-1 vectors))

(defn difference
  "Vector difference"
  [vector-1 & vectors]
  (reduce (partial mapv -) vector-1 vectors))

(defn scalar-multiply
  "Multiplies vector by scalar"
  [vector scalar]
  (mapv (partial * scalar) vector))
