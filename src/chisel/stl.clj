(ns chisel.stl
  "STL generation namespace"
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [chisel.coordinates :as c]))

(defn- facet-normal
  "Calculates vortex unit normal"
  [[v1 v2 v3]]
  (let [v1->v2 (c/difference v2 v1)
        v1->v3 (c/difference v3 v1)
        x1     (v1->v2 0)
        y1     (v1->v2 1)
        z1     (v1->v2 2)
        x2     (v1->v3 0)
        y2     (v1->v3 1)
        z2     (v1->v2 2)]
    (c/scale-vector (c/v [(- (* y1 z2) (* z1 y2))
                          (- (* z1 x2) (* x1 z2))
                          (- (* x1 y2) (* y1 x2))])
                    1)))

(defn- tesselate-patch
  "Tesselates patch represented by sequence of patch slices"
  [patch-slices & {:keys [triangulate-fn] :or {triangulate-fn identity}}]
  (mapcat (fn [[lower-slice upper-slice]]
            (assert (= (count lower-slice)
                       (count upper-slice))
                    "Slices in patch for tesselation must have the same amount of points")
            (mapcat (fn [[lower-point-1 lower-point-2]
                         [upper-point-1 upper-point-2]]
                      [(triangulate-fn [lower-point-1 upper-point-1 lower-point-2])
                       (triangulate-fn [upper-point-2 lower-point-2 upper-point-1])])
                    (partition 2 1 lower-slice)
                    (partition 2 1 upper-slice)))
          (partition 2 1 patch-slices)))

(defn- format-coordinates [v]
  (format "%f %f %f" (v 0) (v 1) (v 2)))

(defn- generate-facet-str
  [[v1 v2 v3 :as vertexes]]
  (format "facet normal %s
    outer loop
      vertex %s
      vertex %s 
      vertex %s
    endloop 
  endfacet\n  "
          (-> vertexes facet-normal format-coordinates)
          (format-coordinates v1)
          (format-coordinates v2)
          (format-coordinates v3)))

(defn generate-ascii-solid
  "Generates ASCII STL solid string"
  [slices]
  (format "solid Chisel_Model\n  %sendsolid Chisel_Model"
          (apply str (tesselate-patch slices :triangulate-fn generate-facet-str))))

;; Utility fn to write to file
(defn write-to-file [path & content]
  (with-open [w (io/writer path)]
    (.write w (string/join "\n" content))))

(def write (partial write-to-file "/Users/janherich/CAD/chisel.stl"))
