(ns chisel.surface-infills
  "Surface Infill implementations, creating non-enclosed surface mesh"
  (:require [chisel.protocols :as protocols]
            [chisel.curves :as curves]
            [chisel.coordinates :as c]
            [chisel.utils :as u]))

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
  [top-patch bottom-patch x-corrugations x-resolution y-corrugations y-resolution
   & {:keys [top-skin? bottom-skin?] :or {top-skin? false bottom-skin? false}}]
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
