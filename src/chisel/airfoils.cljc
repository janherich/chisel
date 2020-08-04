(ns chisel.airfoils
  "Various airfoil curves/patches"
  (:require [chisel.curves :as curves]
            [chisel.protocols :as protocols]
            [chisel.coordinates :as c]
            [chisel.conic-sections :as conics]
            [chisel.projections :as p]))

(defn airfoil
  "Simple airfoil as composite bezier curve"
  [{:keys [front rear thickness max-thickness rear-slope]
    :or {max-thickness 1/3 rear-slope 2/3}}]
  (let [front->rear     (c/difference rear front)
        thickness-value (/ (* thickness (c/vector-length front->rear)) 2)
        chord->top      (c/scale-vector (c/orthogonal-vector front->rear) thickness-value)
        front-up        (protocols/linear-transform front (c/translate-matrix chord->top))
        middle-up       (protocols/linear-transform (c/linear-combination max-thickness front rear)
                                                    (c/translate-matrix chord->top))
        rear-up         (protocols/linear-transform (c/linear-combination rear-slope front rear)
                                                    (c/translate-matrix chord->top))]
    (curves/composite-bezier-curve [[front front-up middle-up]
                                    [middle-up rear-up rear]])))
(comment
  (defn wrap-flanges [airfoil-curve thickness front rear]
    (let [front->rear (c/difference rear front)
          top->down   (c/scale-vector (c/orthogonal-vector front->rear :counterclockwise? true)
                                      thickness)
          front-down  (protocols/translate front top->down)
          rear-down   (protocols/translate rear top->down)]
      (update airfoil-curve :control-points-sequence
              (fn [cps]
                (let [first-airfoil-point (ffirst cps)
                      last-airfoil-point  (last (last cps))]
                  (into [[last-airfoil-point rear]
                         [rear rear-down]
                         [rear-down front-down]
                         [front-down front]
                         [front first-airfoil-point]]
                        cps)))))))
(comment
  (def elliptic-blade
    (let [curves [(conics/elliptic-curve [0 0 0] [28 0 0] [0 0 120] :section :quarter)
                  (conics/elliptic-curve [0 0 0] [-34 0 0] [0 0 120] :section :quarter)]]
      (curves/tensor-product-patch
       (fn [[front rear]]
         (airfoil {:front front :rear rear :thickness 10/83
                   :max-thickness 1/2 :rear-slope 3/4}))
       curves)))

  (def blade-base
    (let [curves [(conics/elliptic-curve [0 0 0] [33 0 0] [0 -125 0] :section :quarter)
                  (conics/elliptic-curve [0 0 0] [-39 0 0] [0 -125 0] :section :quarter)
                  (conics/elliptic-curve [0 0 -2] [-39 0 -2] [0 -125 -2] :section :quarter)
                  (conics/elliptic-curve [0 0 -2] [-37 0 -2] [0 -123 -2] :section :quarter)
                  (conics/elliptic-curve [0 0 -2] [31 0 -2] [0 -123 -2] :section :quarter)
                  (conics/elliptic-curve [0 0 -2] [33 0 -2] [0 -125 -2] :section :quarter)
                  (conics/elliptic-curve [0 0 -2] [0 0 2] [0 -123 -2] :section :quarter)]]
      (curves/tensor-product-patch (fn [[p1 p2 p3 p4 p5 p6 p7]]
                                     (curves/composite-bezier-curve [[p1 p2]
                                                                     [p2 p3]
                                                                     [p3 p4]
                                                                     [p4 p7 p5]
                                                                     [p5 p6]
                                                                     [p6 p1]]))
                                   curves)))

  (defn angle->offset
    "Computes offset of symmetric impluse turbine blade"
    [inlet-angle channel-width]
    (/ channel-width (Math/sin (Math/toRadians inlet-angle))))

  (defn offset->angle
    "Computes angle symmetric impluse turbine blade"
    [offset channel-width]
    (Math/toDegrees (Math/asin (/ channel-width offset))))

  (defn symmetric-impulse-blade
    "Symmetric impulse turbine blade as a composite bezier curve"
    [{:keys [inlet-angle channel-width blade-width edge-radius] :or {edge-radius (/ channel-width 10)}}]
    (let [inlet-angle-rad     (Math/toRadians inlet-angle)
          curvature-angle     (- 180 (* 2 inlet-angle))
          suction-side-length (/ channel-width (Math/tan inlet-angle-rad))
          suction-side-x      (* (Math/sin inlet-angle-rad) suction-side-length)
          suction-side-y      (* (Math/cos inlet-angle-rad) suction-side-length)
          blade-half-width    (/ blade-width 2)
          left-lower-point    [(- blade-half-width) 0]
          left-radius-point   (protocols/translate left-lower-point
                                                   (c/scale-vector
                                                    (c/orthogonal-vector [suction-side-x suction-side-y])
                                                    edge-radius))
          left-upper-point    [(- (- blade-half-width  suction-side-x)) suction-side-y]
          right-lower-point   [blade-half-width 0]
          right-radius-point  (protocols/translate right-lower-point
                                                   (c/scale-vector
                                                    (c/orthogonal-vector [suction-side-x (- suction-side-y)])
                                                    edge-radius))
          right-upper-point   [(- blade-half-width suction-side-x) suction-side-y]]
      (curves/composite-bezier-curve (concat [[left-lower-point left-upper-point]]
                                             (conics/circle-arc
                                              left-upper-point right-upper-point curvature-angle)
                                             [[right-upper-point right-lower-point]]
                                             (conics/circle-arc
                                              right-lower-point right-radius-point 180)
                                             (conics/circle-arc
                                              right-radius-point left-radius-point curvature-angle
                                              :counterclockwise? true)
                                             (conics/circle-arc
                                              left-radius-point left-lower-point 180))))))

(comment
  (require '[chisel.open-scad :as os])

  (defn blade-pair [inlet-angle channel-width blade-width]
    (let [offset (angle->offset inlet-angle channel-width)]
      (os/write
       (os/generate-polygon
        (protocols/points
         (symmetric-impulse-blade {:inlet-angle inlet-angle
                                   :channel-width channel-width
                                   :blade-width blade-width})
         1000))
       (os/generate-polygon
        (protocols/points
         (protocols/translate
          (symmetric-impulse-blade {:inlet-angle inlet-angle
                                    :channel-width channel-width
                                    :blade-width blade-width})
          [0 offset])
         1000)))))

  (blade-pair 10 20 100)

  (require '[chisel.stl :as stl])

  (defn render-turbine [{:keys [inlet-angle channel-width
                                blade-width blade-height blade-count]}]
    (let [bottom-length (angle->offset inlet-angle channel-width)
          inner-radius  (/ (* blade-count bottom-length) 2 Math/PI)
          outer-radius  (+ inner-radius blade-height)
          top-angle     (offset->angle (* bottom-length (/ outer-radius inner-radius))
                                       channel-width)
          rotate-step   (* 2 Math/PI (/ 1 blade-count))
          disk-offset   (+ (/ blade-width 2) (/ blade-width 100))]
      (letfn [(blade-polyhedron [angle]
                [(map (comp (partial p/rotate angle) p/to-cylinder #(conj % outer-radius))
                      (protocols/points (symmetric-impulse-blade {:inlet-angle top-angle
                                                                  :channel-width channel-width
                                                                  :blade-width blade-width})
                                        500))
                 (map (comp (partial p/rotate angle) p/to-cylinder #(conj % inner-radius))
                      (protocols/points (symmetric-impulse-blade {:inlet-angle inlet-angle
                                                                  :channel-width channel-width
                                                                  :blade-width blade-width})
                                        500))])
              (blade-disk [thickness radius]
                (let [curves [(conics/elliptic-curve [disk-offset 0 0]
                                                     [disk-offset 0 radius]
                                                     [disk-offset radius 0])
                              (conics/elliptic-curve [(- disk-offset) 0 0]
                                                     [(- disk-offset) 0 radius]
                                                     [(- disk-offset) radius 0])
                              (conics/elliptic-curve [(- disk-offset) 0 0]
                                                     [(- disk-offset) 0 (- radius thickness)]
                                                     [(- disk-offset) (- radius thickness) 0])
                              (conics/elliptic-curve [disk-offset 0 0]
                                                     [disk-offset 0 (- radius thickness)]
                                                     [disk-offset (- radius thickness) 0])]]
                  (protocols/patch-points
                   (curves/tensor-product-patch #(curves/composite-bezier-curve (partition 2 1 %))
                                                curves)
                   4 1000)))]
        (apply stl/write
               (stl/generate-ascii-solid (blade-disk 2 inner-radius))
               (stl/generate-ascii-solid (blade-disk 1 (+ 1 outer-radius)))
               (map (fn [i]
                      (stl/generate-ascii-solid (blade-polyhedron (* i rotate-step))))
                    (range blade-count)))))))
