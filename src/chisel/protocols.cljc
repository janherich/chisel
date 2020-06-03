(ns chisel.protocols)

(defprotocol PTransformable
  "Protocols for structures which can be object of transformations"
  (linear-transform [this matrix]
    "Given transform `matrix`, transforms object according to it"))

(defprotocol PParametricCurve
  "Protocol for parametric curves"
  (point [this t]
    "given parameter value `t`, returns point on the curve")
  (points [this points-count]
    "given `points-count`, returns ordered sequence of points on the curve with given number of elements")
  (closed? [this]
    "Returns `true` if curve is closed, `false` otherwise"))

(defprotocol PParametricPatch
  "Protocol for parametric patches"
  (slice-points [this t slice-points-count]
    "Give parameter value `t`, returns patch slice with `slice-points-count` points when cut at `t`")
  (patch-points [this slice-points-count slices-count]
    "given `slices-count`, returns ordered sequence of slices (each with `slice-points-count` points) through the patch"))
