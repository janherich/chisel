(ns chisel.open-scad
  "OpenSCAD interoperability methods"
  (:require [clojure.string :as string]
            [chisel.utils :as u]))

;; 2D Open Scad primitives
(defn generate-polygon
  "Generates OpenSCAD polygon code"
  [points]
  (letfn [(format-coordinates [v]
            (format "[%f,%f]" (double (v 0)) (double (v 1))))]
    (format "polygon(points=[%s]);"
            (string/join "," (map format-coordinates points)))))

;; 3D Open Scad primitives
(defn generate-polyhedron
  "Generates OpenSCAD polyhedron code"
  [{:keys [points faces]}]
  (letfn [(format-coordinates [v]
            (format "[%f,%f,%f]" (double (v 0)) (double (v 1)) (double (v 2))))
          (format-indexes [indexes]
            (format "[%s]" (string/join "," indexes)))]
    (format "polyhedron(points=[%s],faces=[%s]);"
            (string/join "," (map format-coordinates points))
            (string/join "," (map format-indexes faces)))))

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

(def write (partial u/write-to-file "/Users/janherich/CAD/chisel.scad"))
