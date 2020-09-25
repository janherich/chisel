(ns chisel.aero
  "Wings, airplanes, aero shapes"
  (:require [chisel.protocols :as protocols]
            [chisel.curves :as curves]
            [chisel.conic-sections :as conics]
            [chisel.coordinates :as c]
            [chisel.surface-infills :as surface-infills]
            [chisel.utils :as u]
            [chisel.open-scad :as os]
            [chisel.stl :as stl]))

(def wing
  (let [top-airfoil    (curves/bezier-curve [(c/v [0 0])
                                             (c/weighted (c/v [0 20]) 1/2)
                                             (c/v [100 0])])
        bottom-airfoil (curves/bezier-curve [(c/v [0 0])
                                             (c/weighted (c/v [0 -10]) 1/2)
                                             (c/v [100 0])])
        top-patch      (curves/bezier-patch [top-airfoil
                                             (-> top-airfoil
                                                 (protocols/linear-transform (c/translate-matrix [0 0 10 #_300]))
                                                 #_(protocols/linear-transform (c/scale-matrix {:x 1/2 :y 1/2})))])
        bottom-patch   (curves/bezier-patch [bottom-airfoil
                                             (-> bottom-airfoil
                                                 (protocols/linear-transform (c/translate-matrix [0 0 10 #_300]))
                                                 #_(protocols/linear-transform (c/scale-matrix {:x 1/2 :y 1/2})))])
        le-end-point   2/10
        te-start-point 19/20]
    (u/merge-triangle-meshes
     (protocols/triangle-mesh top-patch [200 2])
     (protocols/triangle-mesh bottom-patch [200 2])
     #_(surface-infills/x-diagonal-infill top-patch bottom-patch 1 3 150 :across? true))
    #_(u/merge-triangle-meshes
       (protocols/triangle-mesh (curves/cut-patch top-patch [0 le-end-point]) [50 2])
       (protocols/triangle-mesh (curves/cut-patch bottom-patch [0 le-end-point]) [50 2])
       (surface-infills/grid-infill
        (curves/cut-patch top-patch [le-end-point te-start-point])
        (curves/cut-patch bottom-patch [le-end-point te-start-point])
        5 2 10 100)
       #_(surface-infills/connect-surfaces top-patch bottom-patch
                                           (c/v [le-end-point 0])
                                           (c/v [le-end-point 1])
                                           2)
       #_(surface-infills/corrugated-surface
          (curves/cut-patch bottom-patch [le-end-point te-start-point])
          (curves/cut-patch top-patch [le-end-point te-start-point])
          1 2 6 100)
       #_(surface-infills/connect-surfaces top-patch bottom-patch
                                           (c/v [te-start-point 0])
                                           (c/v [te-start-point 1])
                                           2)
       (protocols/triangle-mesh (curves/cut-patch top-patch [te-start-point 1]) [50 2])
       (protocols/triangle-mesh (curves/cut-patch bottom-patch [te-start-point 1]) [50 2]))))
