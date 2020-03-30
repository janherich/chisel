(ns chisel.core
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [chisel.protocols :as p]))

;; points
(def ^:const X_INDEX 0)
(def ^:const Y_INDEX 1)
(def ^:const Z_INDEX 2)

(def ^:const axis->index
  {:x X_INDEX
   :y Y_INDEX
   :z Z_INDEX})

;; Circle
(def ^:const full-circle (* 2 Math/PI))

(defn negate [n]
  (- n (* 2 n)))

(defn mirror-point [axis point]
  (let [axis-index (get axis->index axis)]
    (assert axis-index
            (format "Non valid axis %s given - only %s axis are supported"
                    axis (keys axis->index)))
    (assert (> (count point) axis-index)
            (format "Operation on axis %s not supported on point %s"
                    axis point))
    (update point axis-index negate)))

(defn translate-point [translate-vector point]
  (mapv + point translate-vector))

(defn rotate-point
  ([angle-deg point]
   (rotate-point angle-deg [0 0] point))
  ([angle-deg [pivot-x pivot-y] [x y]]
   (let [angle (* angle-deg (/ Math/PI 180))]
     [(+ pivot-x (- (* (- x pivot-x) (Math/cos angle)) (* (- y pivot-y) (Math/sin angle))))
      (+ pivot-y (+ (* (- y pivot-y) (Math/cos angle)) (* (- x pivot-x) (Math/sin angle))))])))

;; Specialised 2d constructors
(defn parabolic-y
  "Computes y point on parabolic curve"
  [a x]
  (* a x x))

(defn- natural-steps [from to] (inc (- to from)))

(defn parabolic-curve
  "Computes 2d coordinates for parabolic curve"
  ([a from to]
   (parabolic-curve (natural-steps from to) a from to))
  ([points a from to]
   (let [step (/ (- to from) points)]
     (into []
           (map (juxt identity (partial parabolic-y a)))
           (take (inc points) (iterate (partial + step) from))))))

(defn parabolic-blade
  "Computes 2d coordinates of blade section from two parabolic curves"
  ([outer-a inner-a from to]
   (parabolic-blade (natural-steps from to) outer-a inner-a from to))
  ([points outer-a inner-a from to]
   (let [y-offset (- (parabolic-y outer-a from)
                     (parabolic-y inner-a from))]
     (into (parabolic-curve points outer-a from to)
           (map (fn [[x y]]
                  [x (+ y y-offset)]))
           (drop 1 (butlast (rseq (parabolic-curve points inner-a from to))))))))

(defn- circ-point [r angle]
  [(* r (Math/sin angle))
   (* r (Math/cos angle))])

(defn circle
  "Computes 2d coordinates for circle of given diameter `r` and angular steps `n`.
  Optional `render-height` argument in range `[0 1]` which determines height of the
  rendered circle (1 being the full height)."
  ([r n]
   (circle r n 1))
  ([r n render-height]
   (let [cos-height     (+ -1 (* 2 render-height))
         section-angle  (Math/acos cos-height)
         circle-section (- full-circle (* 2 section-angle))
         step-angle     (/ circle-section n)]
     (into []
           (map (partial circ-point r))
           (take (inc n) (iterate #(- % step-angle) (- full-circle section-angle)))))))

(defn- scale-vector [[x y] desired-length]
  (let [ratio (/ desired-length
                 (Math/sqrt (+ (Math/pow x 2) (Math/pow y 2))))]
    [(* x ratio) (* y ratio)]))

(defn- hypotenuse [side-a side-b]
  (Math/sqrt (+ (Math/pow side-a 2) (Math/pow side-b 2))))

(defn- leg [hypotenuse other-leg]
  (Math/sqrt (- (Math/pow hypotenuse 2) (Math/pow other-leg 2))))

(defn- opposite-point [[f-x f-y] [x y]]
  [(+ f-x (- f-x x))
   (+ f-y (- f-y y))])

(defn distanced-path
  "Path distances by `distance` from `path`"
  ([path distance]
   (distanced-path path distance false))
  ([path distance open?]
   (mapv (fn [[left-point middle-point right-point]]
           (let [left-vector  (scale-vector [(- (get left-point Y_INDEX)
                                                (get middle-point Y_INDEX))
                                             (negate (- (get left-point X_INDEX)
                                                        (get middle-point X_INDEX)))]
                                            distance)
                 right-vector (scale-vector [(negate (- (get right-point Y_INDEX)
                                                        (get middle-point Y_INDEX)))
                                             (- (get right-point X_INDEX)
                                                (get middle-point X_INDEX))]
                                            distance)
                 vector-distance (hypotenuse (- (get right-vector X_INDEX)
                                                (get left-vector X_INDEX))
                                             (- (get right-vector Y_INDEX)
                                                (get left-vector Y_INDEX)))
                 shorter-leg (leg distance (/ vector-distance 2))
                 ratio (if (zero? shorter-leg) 1 (/ (/ vector-distance 2) shorter-leg))]
             #_(println left-vector)
             #_(println right-vector)
             #_(println vector-distance)
             (-> middle-point
                 (update X_INDEX +
                         (get left-vector X_INDEX)
                         (* (negate (get left-vector Y_INDEX)) ratio))
                 (update Y_INDEX +
                         (get left-vector Y_INDEX)
                         (* (get left-vector X_INDEX) ratio)))))
         (partition 3 1 (if open?
                          path
                          #_(into [(opposite-point (first path) (second path))]
                                  (conj path (opposite-point (last path) (last (butlast path)))))
                          (into [(last path)]
                                (conj path (first path))))))))

(defn- adjust-rocker [max-rocker l z]
  (+ (- 1 max-rocker)
     (* 4 max-rocker (/ z l) (- 1 (/ z l)))))

(defn- wigley-point-x [fullness l b t z y]
  (* (* b 2)
     (* (/ z l) (- 1 (/ z l)))
     (- 1 (Math/pow (* (/ l t) (/ y l)) fullness))))

(defn wigley-hull-section
  "Computes coordinates of wigley hull section of given length `l`,
  width `b` and depth `t` for given `x`."
  ([l b t z]
   (wigley-hull-section 2 (natural-steps 0 t) l b t z))
  ([fullness number-of-points l b t z]
   (let [step   (/ t number-of-points)
         points (into []
                      (map (juxt (partial wigley-point-x fullness l b t z)
                                 #(* % (adjust-rocker 1/4 l z))
                                 (constantly z)))
                      (take (inc number-of-points) (iterate (partial + step) 0)))]
     (into points (map (partial mirror-point :x)) (rseq points)))))

(defn- fast-point-x [l b t z x]
  (let [ratio (/ b 2 t)]
    (* (/ (* (* b 4)
             (* (/ z l) (- 1 (/ z l))))
          b)
       ratio
       x)))

(defn- fast-point-y [fullness l b t z x]
  (* (- 1 (Math/pow (* (/ l t) (/ x l)) fullness))
     t
     (adjust-rocker 1/4 l z)))

(defn fast-hull-section
  "Computes fast hull section which is kind of 'inverse' of the wigley hull section,
  with parabollic function starting at the buttom of the hull instead of outer edges,
  eq it produces hull with no distinct keel and 'rounded' bottom."
  ([l b t z]
   (fast-hull-section 2 (natural-steps 0 t) l b t z))
  ([fullness number-of-points l b t z]
   (let [step   (/ t number-of-points)
         points (into []
                      (map (juxt (partial fast-point-x l b t z)
                                 (partial fast-point-y fullness l b t z)
                                 (constantly z)))
                      (take (inc number-of-points) (iterate (partial + step) 0)))]
     (into (into [] (rseq points)) (map (partial mirror-point :x)) points))))

(defn straight-taper [breakpoint l z]
  #_(min (/ (/ z l) breakpoint) 1)
  )

(defn- circular-point-x [l b t z y]
  (let [r      (/ b 2)
        offset (- r t)]
    (* (* 4 (/ z l) (- 1 (/ z l)))
       (Math/sqrt (- (Math/pow r 2) (Math/pow (+ offset y) 2))))))

(defn circular-hull-section
  "Computes circular hull section."
  ([l b t z]
   (circular-hull-section (natural-steps 0 t) l b t z))
  ([number-of-points l b t z]
   (let [step   (/ t number-of-points)
         points (into []
                      (map (juxt (partial circular-point-x l b t z)
                                 #(* % (adjust-rocker 1/8 l z))
                                 (constantly z)))
                      (take (inc number-of-points) (iterate (partial + step) 0)))]
     (into points (map (partial mirror-point :x)) (rseq points)))))

(defn extend-coaming
  "Extends hull coaming by given length"
  [coaming-length hull-section-points]
  (into [(update (first hull-section-points) Y_INDEX - coaming-length)]
        (conj hull-section-points
              (update (last hull-section-points) Y_INDEX - coaming-length))))

(defn- ramp-fn [[up-front down-front down-back up-back] z]
  (let [front-diff (- down-front up-front)
        back-diff  (- up-back down-back)]
    (- (/ (- (min (max z up-front) down-front) up-front)
          front-diff)
       (/ (- (min (max z down-back) up-back) down-back)
          back-diff))))

(defn deck-hole
  "Generates ramped rectangular hole in the flat deck"
  [{:keys [ramp width depth]} hull-section-points]
  (let [[x _ z]   (get hull-section-points 0)
        real-sink (* depth (ramp-fn ramp z))]
    (let [x-offset (if (> x (/ width 2))
                     (- x (/ width 2))
                     0)]
      (into [(translate-point (first hull-section-points) [(negate x-offset) real-sink 0])
             (translate-point (first hull-section-points) [(negate x-offset) 0 0])]
            (conj hull-section-points
                  (translate-point (last hull-section-points) [x-offset 0 0])
                  (translate-point (last hull-section-points) [x-offset real-sink 0]))))))

(defn- capture-with-index [pred indexed]
  (loop [idx       0
         remaining indexed]
    (let [current (first remaining)]
      (when current
        (if (pred current)
          (recur (inc idx) (rest remaining))
          [(get indexed (dec idx)) idx])))))

(defn- spar-y [x [_ point-before-y] [point-after-x point-after-y]]
  (if (or (not point-before-y)
          (= x point-after-x))
    point-after-y
    (/ (+ point-before-y point-after-y) 2)))

(defn spar-section
  "Generates spar section (polygon) for given enclosed polygon, returns nil if spar can't be generated"
  ([spar-width section-points]
   (spar-section spar-width 0 section-points))
  ([spar-width connection-distance section-points]
   (let [spar-half-width (/ spar-width 2)]
     (when-let [[upper-smaller outer-idx] (capture-with-index
                                           (fn [[x y]]
                                             (< x spar-half-width))
                                           section-points)]
       (let [upper-bigger (get section-points outer-idx)]
         (when-let [[lower-bigger inner-idx] (capture-with-index
                                              (fn [[x y]]
                                                (> x spar-half-width))
                                              (subvec section-points outer-idx))]
           (let [lower-smaller (get section-points (+ outer-idx inner-idx))
                 y-upper (+ (spar-y spar-half-width upper-smaller upper-bigger)
                            connection-distance)
                 y-lower (- (spar-y spar-half-width lower-bigger lower-smaller)
                            connection-distance)]
             (when (> y-lower y-upper)
               [[spar-half-width y-upper]
                [spar-half-width y-lower]
                [(negate spar-half-width) y-lower]
                [(negate spar-half-width) y-upper]]))))))))

(defn- polygon-area
  "Computes area enclosed by polygon points"
  [points]
  (let [tuples (cons (last points) points)
        products-1 (map *
                        (map first tuples)
                        (map second (rest tuples)))
        products-2 (map *
                        (map first (rest tuples))
                        (map second tuples))
        total      (reduce + 0 (map - products-1 products-2))]
    (/ total
       2)))

(defn- polygon-circumference
  "Computes circumference of polygon points"
  [points]
  (reduce (fn [acc [point next-point]]
            (+ acc
               (Math/sqrt (+ (Math/pow (- (first point) (first next-point)) 2)
                             (Math/pow (- (second point) (second next-point)) 2)))))
          0
          (map vector
               points
               (cons (last points) points))))

(defn naca-4-y [t x]
  (* 5 t (- (+ (* 0.2969 (Math/sqrt x))
               (* 0.2843 (Math/pow x 3)))
            (* 0.1015 (Math/pow x 4))
            (* 0.3516 (Math/pow x 2))
            (* 0.1260 x))))

(defn naca-4
  "Generates NACA 4digit airfoil points"
  ([airfoil-code chord]
   (naca-4 airfoil-code chord (natural-steps 0 chord)))
  ([airfoil-code chord points]
   (naca-4 airfoil-code chord 1 points))
  ([airfoil-code chord render-chord points]
   (let [thickness (* (/ (Integer. (subs airfoil-code 2 4)) 100) chord)
         step      (/ 1 points)
         points    (into []
                         (map (juxt (partial * chord)
                                    (partial naca-4-y thickness)))
                         (take (int (* render-chord (inc points)))
                               (iterate (partial + step) 0)))]
     (into points (map (partial mirror-point :y)) (rseq points)))))

;; 3D computations
(defn extrude-faces
  "Computes faces from sequence of points, where points are presumed
  to describe N polygons with same number of points"
  [n points]
  {:pre [(zero? (mod (count points) n))]}
  (let [full-count (count points)
        points-in-polygon (/ full-count n)]
    (into
     [(into [] (range points-in-polygon))
      (into [] (reverse (range (* points-in-polygon (dec n)) full-count)))]
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
  [& polygon-points-coll]
  {:pre [(apply = (map count polygon-points-coll))]}
  (let [points (apply concat polygon-points-coll)]
    {:points points
     :faces  (extrude-faces (count polygon-points-coll) points)}))

(defn- rectangle-faces
  "Returns 2 faces describing rectangle defined by 4 points in CW direction"
  [lower-right lower-left upper-left upper-right]
  [[lower-right lower-left upper-right]
   [upper-right upper-left lower-left]])

(defn extrude-slices-faces
  "Computes faces from sequences of points, where points are presumed
  to describe N slices, each containing perimeter and inner path with same number of points"
  [n points]
  {:pre [(zero? (mod (count points) n))]}
  (let [hole-spacing-horizontal 2
        hole-spacing-vertical 3
        full-count (count points)
        points-in-slice-path (/ full-count n 2)]
    (into
     []
     (concat
      (mapcat
       (fn [offset]
         (mapcat
          (fn [idx]
            (let [next-idx (mod (inc idx) points-in-slice-path)]
              (rectangle-faces (+ offset idx)
                               (+ offset next-idx)
                               (+ offset next-idx (/ full-count 2))
                               (+ offset idx (/ full-count 2)))))
          (range points-in-slice-path)))
       [0 (* points-in-slice-path (dec n))])
      (mapcat
       (fn [level]
         (let [outer-offset (* level points-in-slice-path)
               inner-offset (* (+ level n) points-in-slice-path)
               holes-in-layer? (zero? (mod level hole-spacing-vertical))]
           (mapcat (fn [idx]
                     (let [next-idx (mod (inc idx) points-in-slice-path)
                           hole?    (and holes-in-layer?
                                         (zero? (mod idx hole-spacing-horizontal)))]
                       (if hole?
                         (reduce into
                                 (rectangle-faces (+ outer-offset next-idx)
                                                    (+ inner-offset next-idx)
                                                    (+ inner-offset next-idx points-in-slice-path)
                                                    (+ outer-offset next-idx points-in-slice-path))
                                 [(rectangle-faces (+ outer-offset idx)
                                                    (+ inner-offset idx)
                                                    (+ inner-offset idx points-in-slice-path)
                                                    (+ outer-offset idx points-in-slice-path))
                                  (rectangle-faces (+ outer-offset next-idx)
                                                   (+ outer-offset idx)
                                                   (+ inner-offset idx)
                                                   (+ inner-offset next-idx))
                                  (rectangle-faces (+ outer-offset next-idx points-in-slice-path)
                                                   (+ outer-offset idx points-in-slice-path)
                                                   (+ inner-offset idx points-in-slice-path)
                                                   (+ inner-offset next-idx points-in-slice-path))])
                         (into (rectangle-faces (+ outer-offset next-idx)
                                                (+ outer-offset idx)
                                                (+ outer-offset idx points-in-slice-path)
                                                (+ outer-offset next-idx points-in-slice-path))
                               (rectangle-faces (+ inner-offset next-idx)
                                                (+ inner-offset idx)
                                                (+ inner-offset idx points-in-slice-path)
                                                (+ inner-offset next-idx points-in-slice-path))))))
                   (range points-in-slice-path))))
       (range (dec n)))))))

(defn extrude-between-slices
  "Generate polyedron from N slices, each describing `:perimeter` and `:inner-path`"
  [& slices]
  {:pre [(apply = (map (comp count :perimeter) slices))
         (apply = (map (comp count :inner-path) slices))]}
  (let [points (concat (mapcat :perimeter slices)
                       (mapcat :inner-path slices))]
    {:points points
     :faces  (extrude-slices-faces (count slices) points)}))

(defn hull-volume
  "Computes volume of the hull"
  [hull-sections]
  (reduce (fn [acc [section-points next-section-points]]
            (let [section-area      (polygon-area section-points)
                  next-section-area (polygon-area next-section-points)
                  height-diff       (- (last (first next-section-points))
                                       (last (first section-points)))]
              (+ acc (* (/ (+ section-area next-section-area) 2)
                        height-diff))))
          0
          (map vector
               hull-sections
               (rest hull-sections))))

(defn hull-surface
  "Computes surface of the hull"
  [hull-sections]
  (reduce (fn [acc [section-points next-section-points]]
            (let [section-circ      (polygon-circumference section-points)
                  next-section-circ (polygon-circumference next-section-points)
                  height-diff       (- (last (first next-section-points))
                                       (last (first section-points)))]
              (+ acc (* (/ (+ section-circ next-section-circ) 2)
                        height-diff))))
          0
          (map vector
               hull-sections
               (rest hull-sections))))

;; Specialised 3d constructors
(defn generate-hull
  "Generates polyhedron of the hull with given length `l`"
  ([section-constructor l b t]
   (generate-hull section-constructor (natural-steps 0 l) l b t))
  ([section-constructor points l b t]
   (let [step     (/ l points)
         sections (map (partial section-constructor l b t)
                       (take (inc points) (iterate (partial + step) 0)))]
     sections
     #_(apply extrude-between-polygons sections))))

(defn generate-spar
  "Generates polyhedron of the hull spar with given thickness `t`"
  ([t hull-sections]
   (generate-spar t 0 hull-sections))
  ([t c hull-sections]
   (let [spar-sections (keep (fn [hull-section]
                               (when-let [spar (spar-section t c hull-section)]
                                 (let [z (last (first hull-section))]
                                   (map #(conj % z) spar))))
                             hull-sections)]
     (apply extrude-between-polygons spar-sections))))

;; 2D Open Scad primitives
(defn generate-polygon
  "Generates OpenSCAD polygon code"
  [points]
  (letfn [(format-coordinates [[x y]]
            (format "[%.3f,%.3f]" (double x) (double y)))]
    (format "polygon(points=[%s]);"
            (string/join "," (map format-coordinates points)))))

;; 3D Open Scad primitives
(defn generate-polyhedron
  "Generates OpenSCAD polyhedron code"
  [{:keys [points faces]}]
  (letfn [(format-coordinates [[x y z]]
            (format "[%.3f,%.3f,%.3f]" (double x) (double y) (double z)))
          (format-indexes [indexes]
            (format "[%s]" (string/join "," indexes)))]
    (format "polyhedron(points=[%s],faces=[%s]);"
            (string/join "," (map format-coordinates points))
            (string/join "," (map format-indexes faces)))))

;; Perimeters DEV
(def cube
  (let [base [[0 0 0]
              [4 0 0]
              [4 4 0]
              [0 4 0]]]
    [{:perimeter base
      :inner-path (distanced-path base 0.5)}
     {:perimeter (mapv #(assoc % 2 1) base)
      :inner-path (distanced-path (mapv #(assoc % 2 1) base) 0.5)}
     {:perimeter (mapv #(assoc % 2 2) base)
      :inner-path (distanced-path (mapv #(assoc % 2 2) base) 0.5)}
     {:perimeter (mapv #(assoc % 2 3) base)
      :inner-path (distanced-path (mapv #(assoc % 2 3) base) 0.5)}
     {:perimeter (mapv #(assoc % 2 4) base)
      :inner-path (distanced-path (mapv #(assoc % 2 4) base) 0.5)}]))

;; Open Scad interoperability
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
(defn write-to-file [path content]
  (with-open [w (io/writer path)]
    (.write w content)))

;; Scooter hull
(defn- in-range [[from to] number]
  (and (>= number from) (<= number to)))

(defn- proportional [[from to] number]
  (let [interval (- to from)]
    (/ (- number from) interval)))

(defn- polynomial [[from to] number]
  (let [interval (* 2 (- to from))
        factor   (- number from)]
    (* 4
       (/ factor interval)
       (- 1 (/ factor interval)))))

(defn- circular [r number]
  (Math/sqrt (- (Math/pow r 2) (Math/pow number 2))))

(defn- adjust-to-range [[from] number]
  (- number from))

(defn- dropping-deck [height horizontal-length center-y circular-length]
  (let [r                (- height center-y)
        horizontal-range [0 horizontal-length]
        circular-end     (+ horizontal-length circular-length)
        circular-range   [(inc horizontal-length) circular-end]
        circular-end-y   (circular r circular-length)
        sloped-end       (Math/round
                          (* (/ circular-end-y circular-length)
                             (+ center-y circular-end-y)))
        sloped-range     [(inc circular-end) (+ circular-end sloped-end)]]
    (fn [number]
      (cond
        (in-range horizontal-range number)
        height

        (in-range circular-range number)
        (+ center-y (circular r (adjust-to-range circular-range number)))

        (in-range sloped-range number)
        (* (+ center-y circular-end-y) (- 1 (proportional sloped-range number)))
        
        :else 0))))

(defn- circular-deck [height front-arc-length center-y]
  (let [r                (- height center-y)
        circular-start-y (circular r front-arc-length)
        sloped-end       (Math/round
                          (* (/ circular-start-y front-arc-length)
                             (+ center-y circular-start-y)))
        sloped-range     [0 sloped-end]
        circular-range   [(inc sloped-end) (+ sloped-end front-arc-length r)]]
    (fn [number]
      (cond
        (in-range sloped-range number)
        (* (+ circular-start-y center-y) (proportional sloped-range number))

        (in-range circular-range number)
        (+ center-y (circular r (adjust-to-range circular-range (inc (- number front-arc-length)))))
        
        :else 0))))

(defn scooter-hull
  [l b t]
  (let [deck-fn     (dropping-deck 200 49 20 175)
        tail-fn     (circular-deck 100 78 20)
        front-slope [0 400]
        rear-slope  [(- (dec l) 100) (dec l)]
        ellipse-factor (/ t b)
        hull (map (fn [z]
                    (let [width-ratio (cond
                                        (in-range front-slope z) (polynomial front-slope z)
                                        #_(in-range rear-slope z) #_(polynomial rear-slope
                                                                            (+ (- l z) (first rear-slope)))
                                        :else 1)]
                      (extend-coaming (if (> z (- l 167))
                                        (negate (tail-fn (- z (- l 167))))
                                        (negate (deck-fn z)))
                                      (mapv (fn [[x y]]
                                              [(* x width-ratio)
                                               (* y ellipse-factor)
                                               z])
                                            (circle (/ b 2) 200 0.5)))))
                  (range 0 l))]
    (difference
     (generate-polyhedron
      (apply extrude-between-polygons hull))
     (format "translate([%s,20,50]) rotate([0,90,0]) cylinder(r=105, h=%s, $fn=100);"
               (negate (/ b 2)) b)
     (format "translate([-30,20,%s]) rotate([0,90,0]) cylinder(r=105, h=%s, $fn=100);"
             (- l 80)
             b))))

;; (scooter-hull 950 150 100)

;; "difference() {%s translate([-10.1,34.95,90]) cube([20.2,20.2,2020]);}" - eli lodka

(defn wrap-frame [content]
  (difference content "translate([-1.7,1.6,100]) cube([3.4,33.4,300]);"))

(def write (partial write-to-file "/Users/janherich/CAD/chisel.scad"))

;; Eli lodka
(comment
  (def hull-sections
    (generate-hull (comp (partial extend-coaming 5)
                         (partial fast-hull-section 4 100))
                   2198 300 100)))

;; Eli Trimaran
(def hull-sections
  (generate-hull
   (comp (partial deck-hole {:ramp [500 540 700 900] :width 100 :depth 50})
         (partial extend-coaming 40)
         (partial circular-hull-section 150))
   2000 160 120))

;; Part 1
(def hull-section-1 ;; 166 ff
  (take 332 (drop 5 hull-sections)))

(def hull-section-2 ;;
  (take 332 (drop 337 hull-sections)))

(def hull-section-3
  (take 332 (drop 669 hull-sections)))

(def hull-section-4
  (take 332 (drop 1001  hull-sections)))

(def hull-section-5
  (take 332 (drop 1333 hull-sections)))

(def hull-section-6
  (take 332 (drop 1665 hull-sections)))

;; Rendering parts 1-5
(comment
  (write
   (difference
    (generate-polyhedron
     (apply extrude-between-polygons hull-section-1))
    (generate-polyhedron
     (generate-spar 7 1.6 hull-section-1)))))

;; Rendering last part with rudder tube
(comment
  (write
   (difference
    (generate-polyhedron
     (apply extrude-between-polygons hull-section-6))
    (generate-polyhedron
     (generate-spar 7 1.6 hull-section-6))
    "translate([0,120,1700]) rotate([90,0,0]) cylinder(r=5.2,h=160,$fn=100);")))

;; Trimaran outrigger float
(def outrigger-float
  (generate-hull (partial wigley-hull-section 3 150)
                 700 70 100))

;; Rendering float half
(comment
  (write
   (generate-polyhedron
    (apply extrude-between-polygons (take 346 (drop 5 outrigger-float))))))
