(ns advent-of-code.day-seventeen 
  (:require
    [clojure.set :as set]))

(defn triangle-number [n]
  (/ (* n (inc n)) 2))

; target area: x=241..275, y=-75..-49
; X Velocity == 22 - final x pos = 253
(triangle-number 22) ; min x velocity
275 ; max x velocity

; Max Y Velocity at 0 == -74
(triangle-number 74) ; max y velocity
-75 ; min y velocity

;target area: x=20..30, y=-10..-5

(defn update-dx [dx]
  (cond 
    (zero? dx) dx
    (pos? dx) (dec dx)
    (neg? dx) (dec dx)))

(defn check-trajectory [box x y dx dy]
  (cond 
    (and (>= (:max-x box) x (:min-x box))
         (>= (:max-y box) y (:min-y box)))
    true
    (or (< y (:min-y box))
        (> x (:max-x box))
        (and (< x (:min-x box)) (zero? dx)))
    false
    :else
    (recur box (+ x dx) (+ y dy) (update-dx dx) (dec dy))))

(let [box {:min-x 241
           :max-x 275
           :min-y -75
           :max-y -49}]
  (count
    (for [dx (range 22 (inc (:max-x box))) 
          dy (range -75 (inc (- (:min-y box)))) 
          :when (check-trajectory box 0 0 dx dy)] 
      {:x dx :y dy})))
