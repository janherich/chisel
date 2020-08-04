(ns chisel.surface-infills
  "Surface Infill implementations, creating non-enclosed surface mesh"
  (:require [chisel.protocols :as protocols]
            [chisel.curves :as curves]
            [chisel.coordinates :as c]
            [chisel.utils :as u]))

(defn corrugated-surface [top-patch bottom-patch
                          x-corrugations x-resolution
                          y-corrugations y-resolution]
  (let [x-step   (/ 1 x-corrugations 2)
        x-ridges (map (fn [x top?]
                        (map (fn [v]
                               (protocols/patch-point (if top? top-patch bottom-patch)
                                                      (v 0) (v 1)))
                             (protocols/polyline (curves/bezier-curve [(c/v [x 0]) (c/v [x 1])])
                                                 x-resolution)))
                      (range 0 (+ 1 x-step) x-step)
                      (iterate not true))
        y-step   (/ 1 y-corrugations 2)
        y-ridges (map (fn [y top?]
                        (map (fn [v]
                               (protocols/patch-point (if top? top-patch bottom-patch)
                                                      (v 0) (v 1)))
                             (protocols/polyline (curves/bezier-curve [(c/v [0 y]) (c/v [1 y])])
                                                 y-resolution)))
                      (range 0 (+ 1 y-step) y-step)
                      (iterate not true))]
    (u/merge-triangle-meshes
     {:points (reduce into [] x-ridges)
      :faces  (u/triangulate-rectangular-mesh (inc (* 2 x-corrugations)) x-resolution)}
     {:points (reduce into [] y-ridges)
      :faces  (u/triangulate-rectangular-mesh (inc (* 2 y-corrugations)) y-resolution)})))
