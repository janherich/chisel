(ns chisel.protocols)

(defprotocol PParametricCurve
  "Protocol for parametric curves"
  (point [this t]
    "given parameter value `t`, returns point on the curve")
  (points [this points-count]
    "given `points-count`, returns ordered sequence of points on the curve with given number of elements"))

(defprotocol PParametricPatch
  "Protocol for parametric patches"
  (slice-points [this t slice-points-count]
    "Give parameter value `t`, returns patch slice with `slice-points-count` points when cut at `t`")
  (patch-points [this slice-points-count slices-count]
    "given `slices-count`, returns ordered sequence of slices (each with `slice-points-count` points) through the patch"))

(defprotocol PHomogenousCoordinate
  "Protocol for homogenous coordinates"
  (coordinates [this]
    "Vector of euclidian coordinate parts")
  (weight [this]
    "Weight of the homogenous coordinate")
  (project [this]
    "Project coordinate to euclidian plane"))
