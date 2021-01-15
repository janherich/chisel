(ns chisel.tools
  "Various tools"
  (:require [chisel.protocols :as protocols]
            [chisel.curves :as curves]
            [chisel.conic-sections :as conics]
            [chisel.coordinates :as c]
            [chisel.utils :as u]
            [chisel.open-scad :as os]
            [chisel.gcode :as gcode]
            [chisel.gcode-layers :as gcode-layers]))


(def rounded-squegee
  (let [base (curves/bezier-curve [(c/v [0 0 0])
                                   (c/v [-150 150 0])
                                   (c/v [150 150 0])
                                   (c/v [0 0 0])])
        top  (curves/clamped-uniform-b-spline
              {:control-points [(c/v [0 0 3])
                                (c/v [0 50 3])
                                (c/v [0 0 3])]
               :order          1})]
    (curves/clamped-uniform-b-spline-patch
     {:control-curves [(curves/constant-curve (c/v [0 0 0])) base top]
      :order          1})))

#_(os/write
  (os/generate-polyhedron
   (protocols/triangle-mesh rounded-squegee [100 3])))
