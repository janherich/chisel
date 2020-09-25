(ns chisel.stl
  "STL generation namespace"
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [chisel.coordinates :as c]
            [chisel.utils :as u]))

(defn- facet-normal
  "Calculates vortex unit normal"
  [[v1 v2 v3]]
  (let [v1->v2 (c/difference v2 v1)
        v1->v3 (c/difference v3 v1)]
    (c/scale-vector (u/cross-product v1->v2 v1->v3) 1)))

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
  [{:keys [points faces]}]
  (format "solid Chisel_Model\n  %sendsolid Chisel_Model"
          (apply str (map (fn [[a b c]]
                            (generate-facet-str [(get points a)
                                                 (get points b)
                                                 (get points c)]))
                          faces))))

;; Utility fn to write to file
(defn write-to-file [path & content]
  (with-open [w (io/writer path)]
    (.write w (string/join "\n" content))))

(def write (partial write-to-file "/Users/janherich/CAD/chisel.stl"))
