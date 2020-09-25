(ns chisel.gcode
  "Gcode generation for patches"
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [chisel.protocols :as protocols]
            [chisel.curves :as curves]
            [chisel.coordinates :as c]
            [chisel.utils :as u]))

(def ^:const max-fan-speed 255)

(def ^:private default-segment
  {:connection-move :travel})

;; Source data for layer generation
(comment
  {:slices-descriptor [{:source          slices-1
                        :reversed?       true ;; optional, default false
                        :layer-fn        (fn [segment layer-idx]) ;; optional
                        :connection-move :print} ;; optional, default `:travel` 
                       ]})

;; Print object
(def print-descriptor
  {;;layers            [{:z 0.2
   ;;                    :layer-height 0.2
   ;;                    :segments [{:polyline   [...]
   ;;                                :line-width 0.3
   ;;                                :connection :line}
   ;;                               {:polyline   [...]
   ;;                                :connection :move}]}]
   :skirt-polyline     [[-60 100] [75 100] [75 -100] [-60 -100] [-60 100]]
   :height-range       [0.05 0.3]
   :extrusion-rate     1    ;; extrusion rate for fine-tunning extruded amount
   :filament-diameter  1.75 ;; filament width in mm
   :line-width         0.4  ;; print line width in mm
   :speed              150  ;; print move speed in mm/s
   :start-speed        30   ;; start print move speed in mm/s
   :travel-speed-ratio 3/2  ;; ratio of travel speed to print speed
   :ramp-layers        4    ;; number of layers where speed is gradualy ramped
   :print-temp         230  ;; print temperature in degrees celsius
   :bed-temp           55   ;; bed temperature in degrees celsius
   :fan-speed-ratio    1/2  ;; fan speed as ratio of the 100% (maximum) fan speed
   })

(defn- header
  "Gcode print header, setting positioning, temperature and priming extruder"
  [{:keys [bed-temp print-temp]}]
  (str "M82\n" ;; Absolute extrusion mode
       "G21\n" ;; Programming in millimeters
       "G90\n" ;; Absolute positioning
       "M107 T0\n" ;; Turn off cooling fan
       (str "M190 S"(format "%.1f" (double bed-temp)) "\n") ;; Set bed temperature
       (str "M109 S"(format "%.1f" (double print-temp)) " T0\n") ;; Set hot-end temperature
       "G28\n" ;; Home all axes
       "G92 E0\n" ;; Reset extruder origin
       "G1 F200 E3\n" ;; Extrude 3mm of filament (priming extruder)
       "G92 E0\n" ;; Reset extruder origin
       "M83\n")) ;; Relative extrusion mode

(def ^:private footer
  (str "M140 S0\n" ;; Set bed temperature to 0 and continue without waiting
       "M107 T0\n" ;; Turn off cooling fan
       "M104 S0\n" ;; Set hot-end temperature to 0 and continue without waiting
       "G92 E0\n" ;; Reset exturder origin
       "G91\n" ;; Relative positioning
       "G1 E-1 F300\n" ;; Retract bit of filament
       "G1 Z+0.5 E-5 X-20 Y-20 F9000\n" ;; Move print head away from printed object
       "G28 X0 Y0\n" ;; Home all axes
       "M84\n" ;; Turn off steppers
       "G90\n" ;; Revert to absolute positioning
       "M82\n")) ;; Absolute extrusion mode

(defn- mm-min-feedrate
  "Converts mm/s speed to mm/min formatted feedrate string"
  [mm-s-speed]
  (format "F%d" (int (* 60 mm-s-speed))))

(defn- layers-speed-sequence
  "Lazy sequence of speed for each layer in mm/s units"
  [{:keys [start-speed speed ramp-layers]}]
  (let [speed-diff   (- speed start-speed)
        speed-step   (/ speed-diff ramp-layers)]
    (concat (range start-speed (+ speed speed-step) speed-step)
            (repeat speed))))

(defn- format-fan-speed
  "Converts fan speed in `[0-1]` ratio to `S0-255` fan speed string"
  [fan-speed-ratio]
  (format "S%.1f" (double (* max-fan-speed fan-speed-ratio))))

(defn- fan-speed-sequence
  "Lazy sequence of fan speeds for each layer in 0-1 range ratios - `nil` for layers
  where no fan speed setting is necessary"
  [{:keys [fan-speed-ratio]}]
  (concat [nil fan-speed-ratio] (repeat nil)))

(defn- format-motion-coordinate
  "Formats motion coordinate to 3 decimal places"
  [coordinate]
  (format "%.3f" (double coordinate)))

(defn- format-extruder-coordinate
  "Formats extruder coordinate to 5 decimal places"
  [coordinate]
  (format "%.5f" (double coordinate)))

(defn- travel-move
  "Generates travel move"
  [[x y z] speed]
  (str "G0 " (mm-min-feedrate speed)
       " X" (format-motion-coordinate x)
       " Y" (format-motion-coordinate y)
       (when z (str " Z" (format-motion-coordinate z)))
       "\n"))

(defn- compute-extrude-amount
  "Computes right amount of filament extrusion for given move"
  [{:keys [filament-diameter extrusion-rate line-width]} line-height [from-x from-y] [x y]]
  (let [line-length          (Math/sqrt (+ (Math/pow (- x from-x) 2)
                                           (Math/pow (- y from-y) 2)))
        cubic-mm-amount      (* line-length line-height line-width)
        square-filament-area (* Math/PI (Math/pow (/ filament-diameter 2) 2))]
    (* extrusion-rate (/ cubic-mm-amount square-filament-area))))

(defn- print-move
  "Generates print move - whenever from and target coordinates differ"
  [print-descriptor from-coordinate [x y :as target-coordinate] speed line-height]
  (when (not= from-coordinate target-coordinate)
    (str "G1 " (mm-min-feedrate speed)
         " X" (format-motion-coordinate x)
         " Y" (format-motion-coordinate y)
         " E" (format-extruder-coordinate
               (compute-extrude-amount print-descriptor line-height from-coordinate [x y]))
         "\n")))

(defn- change-layer
  "Generates gcode for changing layer, positioning the nozzle at the start of first layer segment"
  [z {:keys [polyline]} travel-speed]
  (travel-move (conj (first polyline) z) travel-speed))

(defn- segment-gcode
  "Generates gcode for segment"
  [print-descriptor layer-height print-speed travel-speed segments
   {:keys [polyline connection-move next-segment] :as segment}]
  (let [descriptor      (merge print-descriptor segment)
        print-gcode     (apply str
                               (map (fn [[from-coordinate to-coordinate]]
                                      (print-move descriptor from-coordinate to-coordinate print-speed layer-height))
                                    (partition 2 1 polyline)))
        last-coordinate (last polyline)
        next-coordinate (first (get-in segments [next-segment :polyline]))]
    (str print-gcode
         ;; Generate connecting move only when segments are not already connected and move is requested
         (when (not= last-coordinate next-coordinate)
           (condp = connection-move
             :print  (print-move descriptor last-coordinate next-coordinate print-speed layer-height)
             :travel (travel-move next-coordinate travel-speed)
             :none   nil)))))

(defn- layer-gcode
  "Given `print-descriptor`, `layer]`, `layer-speed` and `fan-speed` arguments, 
  generates g-code for layer"
  [{:keys [height-range travel-speed-ratio] :as print-descriptor}
   {:keys [z layer-height segments] :as layer-descriptor}
   print-speed fan-speed]
  (let [travel-speed    (* print-speed travel-speed-ratio)]
    (assert (<= (first height-range) layer-height (last height-range))
            (format "Line height must be within allowed range, layer-height: %s" layer-height))
    (apply str
           (cond-> ""
             fan-speed
             (str "M106 " (format-fan-speed fan-speed) "\n"))
           (change-layer z (first segments) travel-speed)
           (apply str (map (partial segment-gcode print-descriptor layer-height print-speed travel-speed segments)
                           segments)))))

(defn- prepend-skirt
  "Prepends skirt polyline to the first layer"
  [layers skirt-polyline]
  (update-in layers [0 :segments]
             (fn [segments]
               (into [{:polyline        skirt-polyline
                       :connection-move :print
                       :next-segment    1}]
                     (map #(update % :next-segment inc) segments)))))

(defn generate-segment
  "Generates segment"
  [{:keys [reversed? layer-fn] :as segment} layer-idx]
  (cond-> (merge default-segment segment)
    reversed? (update :polyline rseq)
    layer-fn  (layer-fn layer-idx)))

(defn generate-layers
  "Generate layers from slices specification"
  [{:keys [slices-descriptor]}]
  (let [segments-count (count slices-descriptor)
        layer-slices   (apply map
                              (fn [layer-idx & slices]
                                (let [z (:z (first slices))]
                                  (assert (every? (comp (partial = z) :z) slices)
                                          " Z height must be same among segments")
                                  [z (into []
                                           (map-indexed
                                            (fn [segment-idx segment]
                                              (let [segment-descriptor (-> (get slices-descriptor segment-idx)
                                                                           (dissoc :source))]
                                                (-> (merge segment-descriptor segment)
                                                    (generate-segment layer-idx)
                                                    (dissoc :layer-fn)
                                                    (assoc :next-segment (rem (inc segment-idx) segments-count)))))
                                            slices))]))
                              (cons (iterate inc 0)
                                    (map :source slices-descriptor)))]
    (mapv (fn [[[bottom-z _] [top-z segments]]]
            (let [layer-height (- top-z bottom-z)]
              (assert (pos? layer-height) "Z height of the layers must gradually increase")
              {:z            top-z
               :layer-height layer-height
               :segments     segments}))
          (partition 2 1 layer-slices))))

(defn- layers-sequence
  "Returns layer sequence - in case `:layers` key is present in the print-descriptor, 
  extracts it, if not, slices layers based on `:slices` key.
  Takes care of prepending skirt-polyline to the first layer as well"
  [{:keys [layers skirt-polyline] :as print-descriptor}]
  (let [layers (if layers layers (generate-layers print-descriptor))]
    (cond-> layers
        skirt-polyline (prepend-skirt skirt-polyline))))

(defn generate-gcode
  "Generates Gcode"
  [print-descriptor]
  (apply str
         (header print-descriptor)
         (apply str
                (map (partial layer-gcode print-descriptor)
                     (layers-sequence print-descriptor)
                     (layers-speed-sequence print-descriptor)
                     (fan-speed-sequence print-descriptor)))
         footer))

;; Utility fn to write to file
(defn write-to-file [path & content]
  (with-open [w (io/writer path)]
    (.write w (string/join "\n" content))))

(def write (partial write-to-file "/Users/janherich/CAD/chisel.gcode"))
