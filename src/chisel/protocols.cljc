(ns chisel.protocols)

(defprotocol PTransformable
  "Protocols for structures which can be object of transformations"
  (linear-transform [this matrix]
    "Given transform `matrix`, transforms object according to it"))

(defprotocol PParametricCurve
  "Protocol for parametric curves"
  (curve-point [this t]
    "Given parameter value `t`, returns point on the curve")
  (polyline [this points-count]
    "Given `points-count`, returns polyline as ordered sequence of points approximating the curve with given number of elements")
  (closed? [this]
    "Returns `true` if curve is closed, `false` otherwise"))

(defprotocol PControlPointsCurve
  "Protocol for curves constrained by control points"
  (control-points [this]
    "Returns ordered sequence of control points"))

(defprotocol PPatch
  "Protocol for patches"
  (triangle-mesh [this resolution]
    "Given `resolution` vector, returns triangle-mesh approximating patch, with `:points` key containing vector of points and
    `:faces` key containing set of triangle faces, represented by point indexes")
  (perimeter-curves [this]
    "Map of curves creating perimeter of the patch for `:i` and `:j` dimensions"))

(defprotocol PParametricPatch
  "Protocol for parametric patches"
  (patch-slice [this t]
    "Given parameter value `t`, returns patch slice")
  (patch-point [this i j]
    "Given parameter values `i` and `j`, returns point on patch"))

(defprotocol PStichable
  "Protocol for stichable things"
  (stitch [this new-patch]
    "Returns new stitched-patch enriched by `new-patch` piece"))
