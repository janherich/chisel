(ns chisel.misc
  "Various shapes without clear category, tubes, panels, test pieces..."
  (:require [chisel.protocols :as protocols]
            [chisel.curves :as curves]
            [chisel.conic-sections :as conics]
            [chisel.coordinates :as c]
            [chisel.surface-infills :as surface-infills]
            [chisel.utils :as u]
            [chisel.open-scad :as os]
            [chisel.stl :as stl]))

(defn double-walled-tube [diameter wall-thickness height]
  (let [outer-r (/ diameter 2)
        inner-r (- outer-r wall-thickness)
        outer-tube (curves/bezier-patch
                    [(conics/elliptic-curve (c/v [0 0 0])
                                            (c/v [outer-r 0 0])
                                            (c/v [0 outer-r 0]))
                     (conics/elliptic-curve (c/v [0 0 height])
                                            (c/v [outer-r 0 height])
                                            (c/v [0 outer-r height]))])
        inner-tube (curves/bezier-patch
                    [(conics/elliptic-curve (c/v [0 0 0])
                                            (c/v [inner-r 0 0])
                                            (c/v [0 inner-r 0]))
                     (conics/elliptic-curve (c/v [0 0 height])
                                            (c/v [inner-r 0 height])
                                            (c/v [0 inner-r height]))])]
    [outer-tube
     inner-tube]))

(defn corrugated-tube
  ([diameter wall-thickness height [x-rectangle y-rectangle]]
   (let [x-corrugations (int (/ height x-rectangle))
         y-corrugations (int (/ (* Math/PI diameter) y-rectangle))]
     (corrugated-tube diameter wall-thickness height x-corrugations y-corrugations)))
  ([diameter wall-thickness height y-corrugations x-corrugations]
   (let [[p1 p2] (double-walled-tube diameter wall-thickness height)]
     (surface-infills/corrugated-surface p1 p2
      x-corrugations 2 y-corrugations (int (* Math/PI diameter))
      #_:top-skin? true
      #_:bottom-skin? true))))

(defn triangle-tube [diameter wall-thickness height x y resolution]
  (let [[p1 p2] (double-walled-tube diameter wall-thickness height)]
    (surface-infills/triangle-infill p1 p2 x y resolution)))

(defn rectangular-panel [width height thickness]
  (let [bottom-panel (curves/bezier-patch
                      [(curves/bezier-curve [(c/v [0 0]) (c/v [width 0])])
                       (curves/bezier-curve [(c/v [0 height]) (c/v [width height])])])
        top-panel    (protocols/linear-transform bottom-panel (c/translate-matrix [0 0 thickness]))]
    [bottom-panel
     top-panel]))

(def panel-test
  (let [[top bottom] (rectangular-panel 10 10 5)]
    #_(u/merge-triangle-meshes)
    #_(surface-infills/double-corrugated-triangle-infill top bottom 3 3 100)
    (surface-infills/corrugated-surface top bottom 1 2 1 2)
    #_(surface-infills/connect-surfaces top bottom (c/v [0 0]) (c/v [0 1]) 2)
    #_(surface-infills/connect-surfaces top bottom (c/v [1 0]) (c/v [1 1]) 2)))

(def shape-test
  (let [top (curves/bezier-patch
             [(curves/clamped-uniform-b-spline
               {:control-points [(c/v [-60 0 30]) (c/v [-60 0 0]) (c/v [0 0 0]) (c/v [60 0 0]) (c/v [60 0 30])]
                :order          2})
              (curves/clamped-uniform-b-spline
               {:control-points [(c/v [-60 60 30]) (c/v [-60 60 0]) (c/v [0 60 0]) (c/v [60 60 0]) (c/v [60 60 30])]
                :order          2})])
        bottom (curves/bezier-patch
                [(curves/clamped-uniform-b-spline
                  {:control-points [(c/v [-50 0 30]) (c/v [-50 0 10]) (c/v [0 0 10]) (c/v [50 0 10]) (c/v [50 0 30])]
                   :order          2})
                 (curves/clamped-uniform-b-spline
                  {:control-points [(c/v [-50 60 30]) (c/v [-50 60 10]) (c/v [0 60 10]) (c/v [50 60 10]) (c/v [50 60 30])]
                   :order          2})])]
    #_(u/merge-triangle-meshes)
    (surface-infills/corrugated-surface top bottom 20 100 3 50)
    #_(surface-infills/connect-surfaces top bottom (c/v [1 0]) (c/v [1 1]) 2)
    #_(surface-infills/connect-surfaces top bottom (c/v [0 0]) (c/v [0 1]) 2)
    #_(surface-infills/reinforcing-bands top bottom 5 1/5 [101 10])))
