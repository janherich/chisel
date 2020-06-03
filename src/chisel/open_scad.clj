(ns chisel.open-scad
  "OpenSCAD interoperability methods"
  (:require [clojure.string :as string]
            [clojure.java.io :as io]))

;; 2D Open Scad primitives
(defn generate-polygon
  "Generates OpenSCAD polygon code"
  [points]
  (letfn [(format-coordinates [v]
            (format "[%f,%f]" (v 0) (v 1)))]
    (format "polygon(points=[%s]);"
            (string/join "," (map format-coordinates points)))))

;; 3D Open Scad primitives
(defn generate-polyhedron
  "Generates OpenSCAD polyhedron code"
  [{:keys [points faces]}]
  (letfn [(format-coordinates [v]
            (format "[%f,%f,%f]" (v 0) (v 1) (v 2)))
          (format-indexes [indexes]
            (format "[%s]" (string/join "," indexes)))]
    (format "polyhedron(points=[%s],faces=[%s]);"
            (string/join "," (map format-coordinates points))
            (string/join "," (map format-indexes faces)))))

(defn- extrude-faces
  "Computes faces from sequence of points, where points are presumed
  to describe N polygons with same number of points"
  [n points open?]
  {:pre [(zero? (mod (count points) n))]}
  (let [full-count (count points)
        points-in-polygon (/ full-count n)]
    (into
     (if open?
       []
       [(into [] (range points-in-polygon))
        (into [] (reverse (range (* points-in-polygon (dec n)) full-count)))])
     (mapcat
      (fn [level]
        (let [offset (* level points-in-polygon)]
          (mapcat (fn [idx]
                    (let [next-idx (mod (inc idx) points-in-polygon)]
                      [[(+ offset next-idx)
                        (+ offset idx)
                        (+ offset next-idx points-in-polygon)]
                       [(+ offset next-idx points-in-polygon)
                        (+ offset idx)
                        (+ offset idx points-in-polygon)]]))
                  (range points-in-polygon))))
      (range (dec n))))))

(defn extrude-between-polygons
  "Generate polyhedron from N polygons with same number of points"
  [polygon-points-coll & {:keys [open?]}]
  {:pre [(apply = (map count polygon-points-coll))]}
  (let [points (apply concat polygon-points-coll)]
    {:points points
     :faces  (extrude-faces (count polygon-points-coll) points open?)}))

;; Open Scad operations
(defn difference
  "Generates OpenSCAD `difference` call"
  [& items]
  (str "difference() {" (string/join " " items) "}"))

(defn union
  "Generates OpenSCAD `union` call"
  [& items]
  (str "union() {" (string/join " " items) "}"))

(defn intersection
  "Generates OpenSCAD `intersection` call"
  [& items]
  (str "intersection() {" (string/join " " items) "}"))

(defn linear-extrude
  "Generates OpenSCAD `linear_extrude` call"
  [height & items]
  (str (format "linear_extrude(height=%s) {" height) (string/join " " items) "}"))

;; Utility fn to write to file
(defn write-to-file [path & content]
  (with-open [w (io/writer path)]
    (.write w (string/join "\n" content))))

(def write (partial write-to-file "/Users/janherich/CAD/chisel.scad"))
