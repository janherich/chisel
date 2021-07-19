(ns chisel.filament-extruder
  "Filament extruder parts"
  (:require [chisel.protocols :as protocols]
            [chisel.curves :as curves]
            [chisel.coordinates :as c]
            [chisel.utils :as u]
            [chisel.conic-sections :as conics]
            [chisel.open-scad :as os]
            [chisel.gcode :as gcode]
            [chisel.gcode-layers :as gcode-layers]))

(defn impeller []
  (os/write
   (os/cylinder 50 3)))
