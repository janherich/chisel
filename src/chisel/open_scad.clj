(ns chisel.open-scad
  "OpenSCAD interoperability methods"
  (:require [clojure.string :as string]
            [chisel.utils :as u]))

;; Include library
(defn include
  "Generates OpenSCAD include library call"
  [library]
  (format "include <%s>" library))

;; 2D Open Scad primitives
(defn generate-polygon
  "Generates OpenSCAD polygon code"
  [points]
  (letfn [(format-coordinates [v]
            (format "[%f,%f]" (double (v 0)) (double (v 1))))]
    (format "polygon(points=[%s]);"
            (string/join "," (map format-coordinates points)))))

(defn text
  "Generatex OpenSCAD text code"
  [{:keys [text size valign halign]
    :or {size 10
         valign "baseline"
         halign "left"}}]
  (format "text(text = \"%s\", size=%s, valign=\"%s\", halign=\"%s\");" text size valign halign))

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
(defn rotate
  "Generates OpenSCAD `rotate` call"
  [{:keys [x y z] :or {x 0 y 0 z 0}} & items]
  (str (format "rotate([%s,%s,%s]) {" x y z) (string/join " " items) "}"))

(defn translate
  "Generates OpenSCAD `translate` call"
  [{:keys [x y z] :or {x 0 y 0 z 0}} & items]
  (str (format "translate([%s,%s,%s]) {" x y z) (string/join " " items) "}"))

(defn scale
  "Generates OpenSCAD `scale` call"
  [{:keys [x y z] :or {x 1 y 1 z 1}} & items]
  (str (format "scale([%s,%s,%s]) {" x y z) (string/join " " items) "}"))

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
  [{:keys [height] :or {height 1}} & items]
  (str (format "linear_extrude(height=%s) {" height) (string/join " " items) "}"))

(defn rotate-extrude
  "Generates OpenSCAD `rotate_extrude` call"
  [& raw-items]
  (let [[{:keys [resolution] :or {resolution 100}} items]
        (if (map? (first raw-items))
          [(first raw-items)
           (rest raw-items)]
          [nil raw-items])]
    (str (format "rotate_extrude($fn=%s) {" resolution) (string/join " " items) "}")))

(defn hull
  "Generates OpenSCAD `hull` call"
  [& items]
  (str "hull() {" (string/join " " items) "}"))

(defn cylinder
  "Generates OpenSCAD `cylinder` object"
  [{:keys [radius second-radius height resolution center] :or {resolution 100 center false}}]
  (format "cylinder(r1=%s,r2=%s,h=%s,center=%s,$fn=%s);"
          radius (or second-radius radius) height center resolution))

(defn cube
  "Generates OpenSCAD `cube` object"
  [[x y z] & {:keys [center] :or {center false}}]
  (format "cube([%s,%s,%s], center=%s);" x y z center))

(defn sphere
  "Generates OpenSCAD `sphere` object"
  [{:keys [radius resolution] :or {resolution 100}}]
  (format "sphere(r=%s,$fn=%s);" radius resolution))

(defn square
  "Generates OpenSCAD `square` object"
  [[x y]]
  (format "square([%s,%s]);" x y))

(defn circle
  "Generates OpenSCAD `circle` object"
  [{:keys [radius resolution] :or {resolution 100}}]
  (format "circle(r=%s,$fn=%s);" radius resolution))

(def write (partial u/write-to-file "/Users/janherich/CAD/chisel.scad"))
