(ns chisel.surface-infills
  "Surface Infill implementations, creating non-enclosed surface mesh"
  (:require [chisel.protocols :as protocols]
            [chisel.curves :as curves]
            [chisel.coordinates :as c]
            [chisel.utils :as u]))

(defn connect-surfaces
  "Creates triangle mesh of straight connection between 2 patches"
  ([top-patch bottom-patch start end resolution]
   (connect-surfaces top-patch bottom-patch start start end end resolution))
  ([top-patch bottom-patch start-top start-bottom end-top end-bottom resolution]
   (let [top-points    (mapv (fn [v]
                               (protocols/patch-point top-patch (v 0) (v 1)))
                             (protocols/polyline (curves/bezier-curve [start-top end-top])
                                                 resolution))
         bottom-points (mapv (fn [v]
                               (protocols/patch-point bottom-patch (v 0) (v 1)))
                             (protocols/polyline (curves/bezier-curve [start-bottom end-bottom])
                                                 resolution))]
     {:points (into top-points bottom-points)
      :faces  (u/triangulate-rectangular-mesh 2 resolution)})))

(defn- ridges [corrugations resolution top-patch bottom-patch
               & {:keys [across?] :or {across? false}}]
  (let [step  (/ 1 corrugations 2)
        start (if across? #(c/v [0 %]) #(c/v [% 0]))
        end   (if across? #(c/v [1 %]) #(c/v [% 1]))]
    (reduce into []
            (map (fn [t top?]
                   (map (fn [v]
                          (protocols/patch-point (if top? top-patch bottom-patch)
                                                 (v 0) (v 1)))
                        (protocols/polyline (curves/bezier-curve [(start t) (end t)])
                                            resolution)))
                 (range 0 (+ 1 step) step)
                 (iterate not true)))))

(defn- add-skins [polyhedron top-patch bottom-patch resolution add-top? add-bottom?]
  (cond-> polyhedron
    add-top?
    (u/merge-triangle-meshes (protocols/triangle-mesh top-patch resolution))
    add-bottom?
    (u/merge-triangle-meshes (protocols/triangle-mesh bottom-patch resolution))))

(defn corrugated-surface
  "Creates corrugated triangle mesh between 2 patches"
  [top-patch bottom-patch x-corrugations x-resolution y-corrugations y-resolution
   & {:keys [top-skin? bottom-skin?]}]
  (if (and (pos? x-corrugations) (pos? y-corrugations))
    (-> (u/merge-triangle-meshes
         {:points (ridges x-corrugations x-resolution top-patch bottom-patch)
          :faces  (u/triangulate-rectangular-mesh (inc (* 2 x-corrugations)) x-resolution)}
         {:points (ridges y-corrugations y-resolution top-patch bottom-patch :across? true)
          :faces  (u/triangulate-rectangular-mesh (inc (* 2 y-corrugations)) y-resolution)})
        (add-skins top-patch bottom-patch [y-resolution x-resolution] top-skin? bottom-skin?))
    (if (pos? x-corrugations)
      (-> {:points (ridges x-corrugations x-resolution top-patch bottom-patch)
           :faces  (u/triangulate-rectangular-mesh (inc (* 2 x-corrugations)) x-resolution)}
          (add-skins top-patch bottom-patch [y-resolution x-resolution] top-skin? bottom-skin?))
      (-> {:points (ridges y-corrugations y-resolution top-patch bottom-patch :across? true)
           :faces  (u/triangulate-rectangular-mesh (inc (* 2 y-corrugations)) y-resolution)}
          (add-skins top-patch bottom-patch [y-resolution x-resolution] top-skin? bottom-skin?)))))

(defn- ribs [ribs-count resolution top-patch bottom-patch
            & {:keys [across?] :or {across? false}}]
  (let [step  (/ 1 (dec ribs-count))
        start (if across? #(c/v [0 %]) #(c/v [% 0]))
        end   (if across? #(c/v [1 %]) #(c/v [% 1]))]
    (apply u/merge-triangle-meshes
           (map (fn [t]
                  (connect-surfaces top-patch bottom-patch (start t) (end t) resolution))
                (range 0 (+ 1 step) step)))))

(defn reinforcing-bands
  "Creates top-bottom reinforcing bands"
  [top-patch bottom-patch count width resolution]
  (let [outer-step      (/ 1 count)
        inner-step      (* outer-step width)
        starting-points (range 0 1 outer-step)]
    (apply u/merge-triangle-meshes
           (mapcat (fn [start]
                     [(protocols/triangle-mesh
                       (curves/cut-patch top-patch [0 1] [start (+ start inner-step)]) resolution)
                      (protocols/triangle-mesh
                       (curves/cut-patch bottom-patch [0 1] [start (+ start inner-step)]) resolution)])
                   starting-points))))

(defn grid-infill
  "Creates rectangular grid infill between 2 patches"
  [top-patch bottom-patch x-lines x-resolution y-lines y-resolution
   & {:keys [top-skin? bottom-skin?]}]
  (if (and (pos? x-lines) (pos? y-lines))
    (-> (u/merge-triangle-meshes
         (ribs x-lines x-resolution top-patch bottom-patch)
         (ribs y-lines y-resolution top-patch bottom-patch :across? true))
        (add-skins top-patch bottom-patch [y-resolution x-resolution] top-skin? bottom-skin?))
    (if (pos? x-lines)
      (-> (ribs x-lines x-resolution top-patch bottom-patch)
          (add-skins top-patch bottom-patch [y-resolution x-resolution] top-skin? bottom-skin?))
      (-> (ribs y-lines y-resolution top-patch bottom-patch :across? true)
          (add-skins top-patch bottom-patch [y-resolution x-resolution] top-skin? bottom-skin?)))))

(defn- diagonal-ribs [horizontal vertical resolution top-patch bottom-patch]
  (let [horizontal-step (/ 1 horizontal)
        vertical-step   (/ 1 vertical)
        starting-points (for [x (range 0 1 horizontal-step)
                              y (range 0 1 vertical-step)]
                          [x y])]
    (apply u/merge-triangle-meshes
           (mapcat (fn [[x y]]
                     (let [upper-left  (c/v [x y])
                           upper-right (c/v [(+ x horizontal-step) y])
                           lower-right (c/v [(+ x horizontal-step) (+ y vertical-step)])
                           lower-left  (c/v [x (+ y vertical-step)])]
                       [(connect-surfaces top-patch bottom-patch upper-left lower-right resolution)
                        (connect-surfaces top-patch bottom-patch upper-right lower-left resolution)]))
                   starting-points))))

(defn x-diagonal-infill
  "Creates diagonal grid infill between 2 patches"
  [top-patch bottom-patch horizontal-x vertical-x resolution]
  (diagonal-ribs horizontal-x vertical-x resolution top-patch bottom-patch))

(defn- triangle-patch [patch origin x y resolution]
  {:points (map (fn [[x y]]
                  (protocols/patch-point patch x y))
                (u/right-angle-triangle-points origin x y resolution))
   :faces  (u/triangulate-triangle-mesh resolution)})

(defn triangle-infill
  "Creates triangle infill grid between 2 patches"
  [top-patch bottom-patch horizontal-division vertical-division resolution & {:keys [across?]}]
  (let [horizontal-step (/ 1 horizontal-division)
        vertical-step   (/ 1 vertical-division)
        starting-points (for [x (range horizontal-division)
                              y (range vertical-division)]
                          (let [origin-x (* horizontal-step x)
                                origin-y (* vertical-step y)]
                            [[origin-x origin-y] (even? (+ x y))]))]
    (apply u/merge-triangle-meshes
           (mapcat (fn [[[x y] down?]]
                     (let [upper-left  (c/v [x y])
                           upper-right (c/v [(+ x horizontal-step) y])
                           lower-right (c/v [(+ x horizontal-step) (+ y vertical-step)])
                           lower-left  (c/v [x (+ y vertical-step)])]
                       (cond-> [(connect-surfaces top-patch bottom-patch upper-left lower-left resolution)]
                         down?
                         (into [(connect-surfaces top-patch bottom-patch upper-left lower-right resolution)
                                (triangle-patch top-patch [x y] horizontal-step vertical-step resolution)
                                (triangle-patch bottom-patch [(+ x horizontal-step)
                                                              (+ y vertical-step)]
                                                (- horizontal-step)
                                                (- vertical-step) resolution)])
                         (not down?)
                         (into [(connect-surfaces top-patch bottom-patch upper-right lower-left resolution)
                                (triangle-patch bottom-patch [(+ x horizontal-step) y]
                                                (- horizontal-step)
                                                vertical-step resolution)
                                (triangle-patch top-patch [x (+ y vertical-step)]
                                                horizontal-step
                                                (- vertical-step) resolution)]))))
                   starting-points))))

(defn corrugated-triangle-infill
  "Creates corrugated triangle infill grid between 2 patches"
  [top-patch bottom-patch horizontal-division vertical-division resolution]
  (let [horizontal-step (/ 1 horizontal-division)
        vertical-step   (/ 1 vertical-division)
        starting-points (for [x (range horizontal-division)
                              y (range vertical-division)]
                          (let [origin-x (* horizontal-step x)
                                origin-y (* vertical-step y)]
                            [[origin-x origin-y] (even? (+ x y))]))]
    (apply u/merge-triangle-meshes
           (mapcat (fn [[[x y] down?]]
                     (let [upper-left  (c/v [x y])
                           upper-right (c/v [(+ x horizontal-step) y])
                           lower-right (c/v [(+ x horizontal-step) (+ y vertical-step)])
                           lower-left  (c/v [x (+ y vertical-step)])]
                       (cond-> [(connect-surfaces top-patch bottom-patch upper-left lower-left resolution)]
                         down?
                         (into [(connect-surfaces top-patch bottom-patch
                                                  upper-left upper-left lower-left lower-right resolution)
                                (connect-surfaces top-patch bottom-patch upper-left lower-right resolution)
                                (connect-surfaces bottom-patch top-patch
                                                  lower-right lower-right upper-right upper-left resolution)])
                         (not down?)
                         (into [(connect-surfaces bottom-patch top-patch
                                                  upper-right upper-right lower-right lower-left resolution)
                                (connect-surfaces top-patch bottom-patch upper-right lower-left resolution)
                                (connect-surfaces top-patch bottom-patch
                                                  lower-left lower-left upper-left upper-right resolution)]))))
                   starting-points))))

(defn double-corrugated-triangle-infill
  "Creates double corrugated triangle infill grid between 2 patches"
  [top-patch bottom-patch horizontal-division vertical-division resolution]
  (let [horizontal-step (/ 1 horizontal-division)
        vertical-step   (/ 1 vertical-division)
        starting-points (for [x (range horizontal-division)
                              y (range vertical-division)]
                          (let [origin-x (* horizontal-step x)
                                origin-y (* vertical-step y)]
                            [[origin-x origin-y] (even? (+ x y))]))]
    (apply u/merge-triangle-meshes
           (mapcat (fn [[[x y] down?]]
                     (let [upper-left  (c/v [x y])
                           upper-right (c/v [(+ x horizontal-step) y])
                           lower-right (c/v [(+ x horizontal-step) (+ y vertical-step)])
                           lower-left  (c/v [x (+ y vertical-step)])]
                       (cond-> [(connect-surfaces top-patch bottom-patch upper-left lower-left resolution)]
                         down?
                         (into [(connect-surfaces top-patch bottom-patch
                                                  upper-left upper-left lower-left lower-right resolution)
                                (connect-surfaces top-patch bottom-patch
                                                  lower-right lower-right upper-right upper-left resolution)
                                (connect-surfaces top-patch bottom-patch
                                                  upper-right upper-right lower-left lower-right resolution)
                                (connect-surfaces top-patch bottom-patch
                                                  lower-left lower-left upper-right upper-left resolution)])
                         (not down?)
                         (into [(connect-surfaces top-patch bottom-patch
                                                  upper-right upper-right lower-right lower-left resolution)
                                (connect-surfaces top-patch bottom-patch
                                                  lower-left lower-left upper-left upper-right resolution)
                                (connect-surfaces top-patch bottom-patch
                                                  upper-left upper-left lower-right lower-left resolution)
                                (connect-surfaces top-patch bottom-patch
                                                  lower-right lower-right upper-left upper-right resolution)]))))
                   starting-points))))


