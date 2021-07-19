(ns chisel.gcode
  "Gcode generation for patches"
  (:require [chisel.protocols :as protocols]
            [chisel.curves :as curves]
            [chisel.coordinates :as c]
            [chisel.utils :as u]))

(def ^:private full-circle (* 2 Math/PI))

(defn circular-polyline
  "Generates circular polyline with 100 segments of diameter `r`, usefull for generating
  skirt polylines for circular print beds"
  ([r]
   (circular-polyline r 0))
  ([r offset]
   (let [step (/ full-circle 100)]
     (mapv (fn [angle]
             [(* r (Math/sin angle))
              (* r (Math/cos angle))])
           (range (+ offset 0) (+ offset full-circle) step)))))

(def ^:const max-fan-speed 255)

(def ^:private default-segment
  {:connection-move :travel})

;; Source data for layer generation
(comment
  {:slices-descriptor [{:source          slices-1
                        :connection-move :print ;; optional, default `:travel`
                        :layer-fn        (fn [segment layer-idx])}
                       ]})

;; Print object with basic settings
(def print-descriptor
  {:height-range       [0.05 0.4]
   :extrusion-rate     1    ;; extrusion rate for fine-tunning extruded amount
   :filament-diameter  1.75 ;; filament diameter in mm
   :line-width         0.4  ;; print line width in mm
   :travel-speed-ratio 3/2  ;; ratio of travel speed to print speed
   :ramp-layers        4    ;; number of layers where speed is gradualy ramped
   :first-layer-height 0.3  ;; layer height of the first layer, usually thickner for better bed adhesion on uneven beds
   })

;; Print object
(def qqs-print-descriptor
  (merge print-descriptor
         {:skirt-polyline     (circular-polyline 125)
          :speed              200 ;; print move speed in mm/s
          :start-speed        30  ;; start print move speed in mm/s
          :print-temp         230 ;; full speed print temperature in degrees celsius
          :start-print-temp   210 ;; start speed print temperature in degrees celsius
          :bed-temp           55  ;; bed temperature in degrees celsius
          :fan-speed-ratio    1/2 ;; fan speed as ratio of the 100% (maximum) fan speed
          :fan-start-layer    3 ;; start fan from 4th layer (indexing from zero)
          }))

;; Print object
(def bigprinter-print-descriptor
  (merge print-descriptor
         {:skirt-polyline        (circular-polyline 225)
          :speed                 200  ;; print move speed in mm/s
          :start-speed           20   ;; start print move speed in mm/s
          :print-temp            230  ;; print temperature in degrees celsius
          :start-print-temp      210  ;; start speed print temperature in degrees celsius
          :ramp-layers           10   ;; slowly bring up to speed
          :fan-speed-ratio       1 ;; fan speed as ratio of the 100% (maximum) fan speed
          :fan-start-layer       4 ;; start fan from 4th layer (indexing from zero)
          }))

;; Print object
(def deltav1-print-descriptor
  (merge print-descriptor
         {:skirt-polyline        (circular-polyline 175)
          :speed                 120  ;; print move speed in mm/s
          :start-speed           30   ;; start print move speed in mm/s
          :print-temp            230  ;; print temperature in degrees celsius
          :start-print-temp      210  ;; start speed print temperature in degrees celsius
          :ramp-layers           10   ;; slowly bring up to speed
          :fan-speed-ratio       1  ;; fan speed as ratio of the 100% (maximum) fan speed
          :fan-start-layer       4    ;; start fan from 4th layer (indexing from zero)
          }))

;; Print object
(def cr10-print-descriptor
  (merge print-descriptor
         {:skirt-polyline     [[50 50] [250 50] [250 250] [50 250] [50 50]]
          :speed              45   ;; print move speed in mm/s
          :start-speed        25   ;; start print move speed in mm/s
          :print-temp         220 ;; print temperature in degrees celsius
          :bed-temp           55  ;; bed temperature in degrees celsius
          :fan-speed-ratio    1/2 ;; fan speed as ratio of the 100% (maximum) fan speed
          :fan-start-layer    2 ;; start fan from 3rd layer (indexing from zero)
          }))

;; Filament object
(def lw-pla-print-descriptor
  {:print-temp     245
   :extrusion-rate 0.45
   :ramp-layers    20
   :speed          100})

(def gonzales-pla-print-descriptor
  {:print-temp       230
   :start-print-temp 210
   :speed            160})

(def petg-print-descriptor
  {:start-print-temp 230
   :print-temp       250})

(def abs-print-descriptor
  {:start-print-temp 240
   :print-temp       260
   :fan-speed-ratio  nil ;; 1/4
   :speed            150
   ;;:fan-start-layer  8
   })

(def pa12-gf-print-descriptor
  {:start-print-temp 270
   :print-temp       280
   :fan-speed-ratio  nil
   :start-speed      20
   :speed            150
   :ramp-layers      15})

(def ca-pet-print-descriptor
  {:start-print-temp 230
   :print-temp       240
   :fan-speed-ratio  nil
   :start-speed      30
   :speed            60
   :ramp-layers      5})

(defn- format-temperature
  "Formats temperature setting string"
  [temp-celsius]
  (format "S%.1f" (double temp-celsius)))

(defn- header
  "Gcode print header, setting positioning, temperature and priming extruder"
  [{:keys [bed-temp print-temp start-print-temp]}]
  (str "M82\n" ;; Absolute extrusion mode
       "G21\n" ;; Programming in millimeters
       "G90\n" ;; Absolute positioning
       "M107 T0\n" ;; Turn off cooling fan
       (when bed-temp
         (str "M190 " (format-temperature bed-temp) "\n")) ;; Set bed temperature
       (str "M109 " (format-temperature (or start-print-temp print-temp)) " T0\n") ;; Set hot-end temperature
       "G28\n" ;; Home all axes
       "G92 E0\n" ;; Reset extruder origin
       "G1 F200 E3\n" ;; Extrude 3mm of filament (priming extruder)
       "G92 E0\n" ;; Reset extruder origin
       "M83\n")) ;; Relative extrusion mode

(defn- footer
  "Gcode print footer, turning of heaters, etc."
  [{:keys [bed-temp]}]
  (str (when bed-temp "M140 S0\n") ;; Set bed temperature to 0 and continue without waiting
       "M107 T0\n" ;; Turn off cooling fan
       "M104 S0\n" ;; Set hot-end temperature to 0 and continue without waiting
       "G92 E0\n"  ;; Reset exturder origin
       "G91\n"     ;; Relative positioning
       "G1 E-1 F300\n"                  ;; Retract bit of filament
       "G1 Z+0.5 E-5 X-20 Y-20 F9000\n" ;; Move print head away from printed object
       "G28 X0 Y0\n"                    ;; Home all axes
       "M84\n"                          ;; Turn off steppers
       "G90\n"   ;; Revert to absolute positioning
       "M82\n")) ;; Absolute extrusion mode

(defn- mm-min-feedrate
  "Converts mm/s speed to mm/min formatted feedrate string"
  [mm-s-speed]
  (format "F%d" (int (* 60 mm-s-speed))))

(defn- layers-speed-sequence
  "Lazy sequence of speed for each layer in mm/s units"
  [{:keys [start-speed speed ramp-layers]}]
  (if start-speed
    (let [speed-diff   (- speed start-speed)
          speed-step   (/ speed-diff ramp-layers)]
      (concat (range start-speed (+ speed speed-step) speed-step)
              (repeat speed)))
    (repeat speed)))

(defn layers-temp-sequence
  "Lazy sequence of print temperature settings for each layer in celsius units"
  [{:keys [start-print-temp print-temp ramp-layers]}]
  (if start-print-temp
    (let [temp-diff (- print-temp start-print-temp)
          temp-step (/ temp-diff ramp-layers)]
      (concat (range start-print-temp (+ print-temp temp-step) temp-step)
              (repeat nil)))
    (repeat nil)))

(defn- format-fan-speed
  "Converts fan speed in `[0-1]` ratio to `S0-255` fan speed string"
  [fan-speed-ratio]
  (format "S%.1f" (double (* max-fan-speed fan-speed-ratio))))

(defn- fan-speed-sequence
  "Lazy sequence of fan speeds for each layer in 0-1 range ratios - `nil` for layers
  where no fan speed setting is necessary"
  [{:keys [fan-speed-ratio fan-start-layer] :or {fan-start-layer 1}}]
  (concat (repeat fan-start-layer nil) [fan-speed-ratio] (repeat nil)))

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
   print-speed print-temp-setting fan-speed]
  (let [travel-speed    (* print-speed travel-speed-ratio)]
    (assert (<= (first height-range) layer-height (last height-range))
            (format "Line height must be within allowed range, layer-height: %s" layer-height))
    (apply str
           (cond-> ""
             fan-speed
             (str "M106 " (format-fan-speed fan-speed) "\n")
             print-temp-setting
             (str "M104 " (format-temperature print-temp-setting) "\n"))
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

(defn- adjust-first-layer-widths
  "Thicker first layer to promote better bed adhesion"
  [layers first-layer-height line-width]
  (let [original-first-layer-height (get-in layers [0 :layer-height])
        height-difference           (- first-layer-height original-first-layer-height)]
    (mapv (fn [layer]
            (update layer :z + height-difference))
          (assoc-in layers [0 :layer-height] first-layer-height))))

(defn generate-segment
  "Generates segment"
  [{:keys [layer-fn] :as segment} layer-idx]
  (cond-> (merge default-segment segment)
    layer-fn (layer-fn layer-idx)))

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
  [{:keys [layers skirt-polyline line-width first-layer-height] :as print-descriptor}]
  (let [layers (if layers layers (generate-layers print-descriptor))]
    (cond-> layers
      skirt-polyline (prepend-skirt skirt-polyline)
      first-layer-height (adjust-first-layer-widths first-layer-height line-width))))

(defn generate-gcode
  "Generates Gcode"
  [print-descriptor]
  (apply str
         (header print-descriptor)
         (apply str
                (map (partial layer-gcode print-descriptor)
                     (layers-sequence print-descriptor)
                     (layers-speed-sequence print-descriptor)
                     (layers-temp-sequence print-descriptor)
                     (fan-speed-sequence print-descriptor)))
         (footer print-descriptor)))

(def write (partial u/write-to-file "/Users/janherich/CAD/chisel.gcode"))
