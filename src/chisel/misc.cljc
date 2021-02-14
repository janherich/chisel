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

(def ^:private layers-per-cm 50)

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
  (let [[face-1 face-2] (rectangular-panel 400 25 10 [-200 -5])]
    (merge (gcode-layers/corrugated-panel-descriptor face-1 face-2 20
                                                     (int (* layers-per-cm 2.5))
                                                 2)
           {:skirt-polyline (gcode/circular-polyline 205) #_[[50 50] [250 50] [250 250] [50 250] [50 50]]})))

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

(defn effector-panel []
  (let [bottom-panel (curves/bezier-patch
                       [(curves/bezier-curve [(c/v [135 140]) (c/v [135 140 35])])
                        (curves/bezier-curve [(c/v [150 150]) (c/v [150 150 35])])
                        (curves/bezier-curve [(c/v [165 140]) (c/v [165 140 35])])])
        top-panel    (protocols/linear-transform bottom-panel (c/translate-matrix [0 5 0]))]
     (merge (gcode-layers/corrugated-panel-descriptor top-panel
                                                      bottom-panel
                                                      6
                                                      (* 35 5)
                                                      100
                                                      :modulate-curve (gcode-layers/sine-curve 1/20 5))
            {:skirt-polyline [[50 50] [250 50] [250 250] [50 250] [50 50]]})))

(comment
  (u/write-to-file
   "/Users/janherich/CAD/effector_panel.gcode"
   (gcode/generate-gcode (merge gcode/cr10-print-descriptor (effector-panel)))))

(defn rounded-panel [width thickness height]
  (let [half-width     (/ width 2)
        half-thickness (/ thickness 2)
        bottom-panel   (curves/clamped-b-spline-patch
                        {:control-curves [(curves/bezier-curve [(c/v [(- half-width) 0])
                                                                (c/v [(- half-width) 0 height])])
                                          (with-meta
                                            (curves/bezier-curve [(c/v [(- half-width) half-thickness])
                                                                  (c/v [(- half-width) half-thickness height])])
                                            {:weight conics/WEIGHT_90})
                                          (curves/bezier-curve [(c/v [(- (- half-width half-thickness)) half-thickness])
                                                                (c/v [(- (- half-width half-thickness)) half-thickness height])])
                                          (curves/bezier-curve [(c/v [(- half-width half-thickness) half-thickness])
                                                                (c/v [(- half-width half-thickness) half-thickness height])])
                                          (with-meta
                                            (curves/bezier-curve [(c/v [half-width half-thickness])
                                                                  (c/v [half-width half-thickness height])])
                                            {:weight conics/WEIGHT_90})
                                          (curves/bezier-curve [(c/v [half-width 0])
                                                                (c/v [half-width 0 height])])]
                         :order          2
                         :knot-vector    [1/2 1/2 1/2]})
        top-panel      (protocols/linear-transform bottom-panel (c/flip-matrix :y))
        half-radius    (* 1/4 thickness Math/PI)
        straight-edge  (- width thickness)
        infill-start   (/ half-radius (+ (* 2 half-radius) straight-edge))
        corrugations   (int (dec (/ straight-edge thickness)))]
    (merge (gcode-layers/corrugated-panel-descriptor top-panel
                                                     bottom-panel
                                                     corrugations
                                                     (* height 5)
                                                     100
                                                     :skin-line-width 0.4
                                                     :modulate-curve (gcode-layers/sine-curve 1/130 (/ height 40))
                                                     :corrugate-fn (partial gcode-layers/cutoff-corrugations infill-start))
           {:skirt-polyline (gcode/circular-polyline (+ half-width 5))})))

(defn panel-infill [width thickness height]
  (let [half-width               (/ width 2)
        half-thickness           (/ thickness 2)
        [bottom-panel top-panel] (rectangular-panel width height thickness [(- half-width) (- half-thickness)])]
    (merge (gcode-layers/double-corrugated-panel-descriptor top-panel
                                                     bottom-panel
                                                     2 #_(int (dec (/ width thickness)))
                                                     (* height 5)
                                                     2
                                                     :modulate-curve (gcode-layers/sine-curve 1/15 2))
            {:skirt-polyline (gcode/circular-polyline (+ half-width 5))})))

(defn circles-on-bed [max-diameter number]
  {:slices-descriptor (mapv (fn [d]
                              (let [circle-polyline (gcode/circular-polyline (* 10 (/ d 2)))]
                                {:source          [{:z        0
                                                    :polyline circle-polyline}
                                                   {:z        0.2
                                                    :polyline circle-polyline}]
                                 :connection-move :print}))
                            (range (- max-diameter (* 2 number)) (inc max-diameter)))})

(defn tube [r thickness height]
  (let [p1 (c/v [(- r) 0 0])
        p2 (c/v [(- r) r 0])
        p3 (c/v [0 r 0])
        p4 (c/v [r r 0])
        p5 (c/v [r 0 0])
        p6 (c/v [r (- r) 0])
        p7 (c/v [0 (- r) 0])
        p8 (c/v [(- r) (- r) 0])
        
        outer-tube (curves/clamped-b-spline-patch
                    {:control-curves [(curves/bezier-curve [p1 (u/lift-z p1 height)])
                                      (with-meta (curves/bezier-curve [p2 (u/lift-z p2 height)])
                                        {:weight conics/WEIGHT_90})
                                      (curves/bezier-curve [p3 (u/lift-z p3 height)])
                                      (with-meta (curves/bezier-curve [p4 (u/lift-z p4 height)])
                                        {:weight conics/WEIGHT_90})
                                      (curves/bezier-curve [p5 (u/lift-z p5 height)])
                                      (with-meta (curves/bezier-curve [p6 (u/lift-z p6 height)])
                                        {:weight conics/WEIGHT_90})
                                      (curves/bezier-curve [p7 (u/lift-z p7 height)])
                                      (with-meta (curves/bezier-curve [p8 (u/lift-z p8 height)])
                                        {:weight conics/WEIGHT_90})
                                      (curves/bezier-curve [p1 (u/lift-z p1 height)])]
                     :knot-vector    [1/4 1/4 2/4 2/4 3/4 3/4]
                     :order          2})]
    [outer-tube
     (protocols/linear-transform outer-tube (c/scale-matrix {:x (/ r (+ r thickness))
                                                             :y (/ r (+ r thickness))}))]))

(comment
  (let [[o i] (tube 50 10 500)]
    (os/write
     (os/generate-polyhedron
      (protocols/triangle-mesh o [2 200]))
     (os/generate-polyhedron
      (protocols/triangle-mesh i [2 200])))))

(defn tube-gcode [r thickness height]
  (let [[o i] (tube r thickness height)]
    (merge (gcode-layers/ribbed-corrugated-panel-descriptor o i
                                                            (int (/ height thickness 2))
                                                            (int (/ (* Math/PI r) thickness))
                                                            (* height 5)
                                                            200)
           {:skirt-polyline (gcode/circular-polyline (+ r 5))})))
