(ns chisel.ml-optimizer
  "ML playground"
  (:import [clojure.lang IFn])
  (:require [uncomplicate.commons.core :refer [with-release let-release]]
            [uncomplicate.neanderthal
             [native :refer [dv dge]]
             [core :refer [mv! mm! mm axpy! scal! transfer! rk! entry! view-ge]]
             [vect-math :refer [tanh! linear-frac!]]]))

(defprotocol Parameters
  (weights [this])
  (bias [this]))

(deftype FullyConnectedInference [w b activ-fn]
  Parameters
  (weights [this] w)
  (bias [this] b)
  IFn
  (invoke [_ x ones a]
    (activ-fn (rk! -1.0 b ones (mm! 1.0 w x 0.0 a)))))

(defn fully-connected [activ-fn in-dim out-dim]
  (let-release [w    (dge out-dim in-dim)
                bias (dv out-dim)]
    (->FullyConnectedInference w bias activ-fn)))

(defn activ-sigmoid! [x]
  (linear-frac! 0.5 (tanh! (scal! 0.5 x)) 0.5))

(defn activ-tanh! [x]
  (tanh! x))

#_(with-release [x       (dge 2 3 [1 1 0.3 0.9 2 1.1])
               a       (dge 4 3)
               layer-1 (fully-connected activ-sigmoid! 2 4)
               ones    (entry! (dv 3) 1)]
  (transfer! [0.3 0.1 0.0 0.0 0.6 2.0 3.7 1.0] (weights layer-1))
  (transfer! [0.7 0.2 1.1 2] (bias layer-1))
  (layer-1 x ones a)
  (println a))

(def digit-bias (into [] (repeat 4 0.0)))
                 
(def digit-weights [0.0 0.0 0.0 0.0 ;; 0
                    0.0 0.0 0.0 1.0 ;; 1
                    0.0 0.0 1.0 0.0 ;; 2
                    0.0 0.0 1.0 1.0 ;; 3
                    0.0 1.0 0.0 0.0 ;; 4
                    0.0 1.0 0.0 1.0 ;; 5
                    0.0 1.0 1.0 0.0 ;; 6
                    0.0 1.0 1.0 1.0 ;; 7
                    1.0 0.0 0.0 0.0 ;; 8
                    1.0 0.0 0.0 1.0 ;; 9
                    ])

(defn test-digits []
  (with-release [input  (dge 10 10
                             (reduce #(assoc %1 (+ (* %2 10) %2) 1)
                                     (into [] (repeat 100 0))
                                     (range 0 10)))
                 result (dge 4 10)
                 layer  (fully-connected activ-tanh! 10 4)
                 ones   (entry! (dv 10) 1)]
    (transfer! digit-weights (weights layer))
    (transfer! digit-bias (bias layer))
    (layer input ones result)
    (println result)))
