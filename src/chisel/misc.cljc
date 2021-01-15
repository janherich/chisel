(ns chisel.misc
  "Various shapes without clear category, tubes, panels, test pieces..."
  (:require [chisel.protocols :as protocols]
            [chisel.curves :as curves]
            [chisel.conic-sections :as conics]
            [chisel.coordinates :as c]
            [chisel.surface-infills :as surface-infills]
            [chisel.utils :as u]
            [chisel.open-scad :as os]
            [chisel.stl :as stl]
            [chisel.gcode :as gcode]
            [chisel.gcode-layers :as gcode-layers]))

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
      :top-skin? true
      #_:bottom-skin? #_true))))

(defn triangle-tube [diameter wall-thickness height x y resolution]
  (let [[p1 p2] (double-walled-tube diameter wall-thickness height)]
    (surface-infills/triangle-infill p1 p2 x y resolution)))

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

(def ^:private layers-per-cm 40)

(defn rectangular-panel
  ([width height thickness]
   (rectangular-panel width height thickness [0 0]))
  ([width height thickness [x y]]
   (let [bottom-panel (curves/bezier-patch
                       [(curves/bezier-curve [(c/v [x y 0]) (c/v [x y height])])
                        (curves/bezier-curve [(c/v [(+ x width) y 0]) (c/v [(+ x width) y height])])])
         top-panel    (protocols/linear-transform
                       bottom-panel (c/translate-matrix [0 thickness 0]))]
     [bottom-panel
      top-panel])))

(defn panel-gcode []
  (let [[face-1 face-2] (rectangular-panel 100 100 10 [100 150])]
    (merge (gcode-layers/corrugated-panel-descriptor face-1 face-2 10
                                                 (* layers-per-cm 10)
                                                 2)
           {:skirt-polyline [[50 50] [250 50] [250 250] [50 250] [50 50]]})))


(defn curved-closed-panel
  [radius thickness height]
  (let [bottom-r     (- radius thickness)
        top-panel    (curves/clamped-b-spline-patch
                      {:control-curves [(curves/bezier-curve [(c/v [0 0 0]) (c/v [0 0 height])])
                                        (curves/bezier-curve [(c/v [0 0 0]) (c/v [0 0 height])])
                                        (curves/bezier-curve [(c/v [(- radius) 0 0]) (c/v [(- radius) 0 height])])
                                        (with-meta
                                          (curves/bezier-curve [(c/v [(- radius) radius 0]) (c/v [(- radius) radius height])])
                                          {:weight conics/WEIGHT_90})
                                        (curves/bezier-curve [(c/v [0 radius 0]) (c/v [0 radius height])])
                                        (with-meta
                                          (curves/bezier-curve [(c/v [radius radius 0]) (c/v [radius radius height])])
                                          {:weight conics/WEIGHT_90})
                                        (curves/bezier-curve [(c/v [radius 0 0]) (c/v [radius 0 height])])
                                        (curves/bezier-curve [(c/v [0 0 0]) (c/v [0 0 height])])
                                        (curves/bezier-curve [(c/v [0 0 0]) (c/v [0 0 height])])]
                       :knot-vector    [1/4 1/4 2/4 2/4 3/4 3/4]
                       :order          2})
        bottom-panel (curves/clamped-b-spline-patch
                      {:control-curves [(curves/bezier-curve [(c/v [0 thickness 0]) (c/v [0 thickness height])])
                                        (curves/bezier-curve [(c/v [0 thickness 0]) (c/v [0 thickness height])])
                                        (curves/bezier-curve [(c/v [(- bottom-r) thickness 0]) (c/v [(- bottom-r) thickness height])])
                                        (with-meta
                                          (curves/bezier-curve [(c/v [(- bottom-r) bottom-r 0]) (c/v [(- bottom-r) bottom-r height])])
                                          {:weight conics/WEIGHT_90})
                                        (curves/bezier-curve [(c/v [0 bottom-r 0]) (c/v [0 bottom-r height])])
                                        (with-meta
                                          (curves/bezier-curve [(c/v [bottom-r bottom-r 0]) (c/v [bottom-r bottom-r height])])
                                          {:weight conics/WEIGHT_90})
                                        (curves/bezier-curve [(c/v [bottom-r thickness 0]) (c/v [bottom-r thickness height])])
                                        (curves/bezier-curve [(c/v [0 thickness 0]) (c/v [0 thickness height])])
                                        (curves/bezier-curve [(c/v [0 thickness 0]) (c/v [0 thickness height])])]
                       :knot-vector    [1/4 1/4 2/4 2/4 3/4 3/4]
                       :order          2})]
    #_(os/write
      (os/generate-polyhedron
       (protocols/triangle-mesh top-panel [2 200]))
      (os/generate-polyhedron
       (protocols/triangle-mesh bottom-panel [2 200])))
    (gcode-layers/ribbed-panel-descriptor top-panel bottom-panel
                                          (int (Math/ceil (/ height thickness 3)))
                                          (* height 5)
                                          200)))
