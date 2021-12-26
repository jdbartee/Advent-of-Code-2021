(ns advent-of-code.day-twenty-two 
  (:require
    [clojure.string :as string]))

(defn parse-range-def [range-def]
  (let [defin (string/join (drop 2 range-def))
        [s e] (string/split defin #"\.\." 2)
        start (Integer/parseInt s)
        end (Integer/parseInt e)]
    {:start start :end end}))

(defn parse-line [line]
  (let [[state pos-text] (string/split line #" " 2)
        [xdef ydef zdef] (string/split pos-text #"," 3)]
    {:state (= state "on")
     :x (parse-range-def xdef)
     :y (parse-range-def ydef)
     :z (parse-range-def zdef)}))

(defn concerned-points [start end]
  (for [x (range start (inc end)) 
        y (range start (inc end)) 
        z (range start (inc end))]
    {:x x :y y :z z}))

(defn apply-rule-to-point [point state rule]
  (let [{px :x
         py :y
         pz :z} point
        {rule-state :state
         {xmin :start
          xmax :end} :x
         {ymin :start
          ymax :end} :y
         {zmin :start
          zmax :end} :z} rule]
    
    (if (and (<= xmin px xmax)
             (<= ymin py ymax)
             (<= zmin pz zmax))
      rule-state
      state)))

(defn count-ons [points rules]
  (->>
    (map
      #(reduce (partial apply-rule-to-point %) false rules)
      points)
    (filter true?)
    (count)))

(defn conditional-axis [s e]
  (if (<= s e)
    [{:start s :end e}]
    []))

(defn axises-intersect? [a b]
  (let [{a1 :start a2 :end} a
        {b1 :start b2 :end} b]
    (or (<= b1 a1 b2)
        (<= b1 a2 b2)
        (<= a1 b1 a2)
        (<= a1 b2 a2))))

(defn break-apart-axis [a b]
      (let [{a1 :start a2 :end} a
            {b1 :start b2 :end} b
            
            p1 (max a1 b1)
            p2 (min a2 b2)]
        (concat
          (conditional-axis a1 (dec b1))
          (conditional-axis (inc b2) a2)
          (conditional-axis p1 p2))))
      

(defn cuboids-intersect? [a b]
  (and (axises-intersect? (:x a) (:x b))
       (axises-intersect? (:y a) (:y b))
       (axises-intersect? (:z a) (:z b))))

(defn break-apart-cuboid [a b]
  (let [xs (break-apart-axis (:x a) (:x b))
        ys (break-apart-axis (:y a) (:y b))
        zs (break-apart-axis (:z a) (:z b))]
    
    (->> (for [x xs y ys z zs]
           {:x x :y y :z z})
         (set)
         (filter (partial cuboids-intersect? a))
         (filter (complement (partial cuboids-intersect? b)))
         (vec))))

(defn cuboid-area [c]
  (let [{{x1 :start x2 :end} :x
         {y1 :start y2 :end} :y
         {z1 :start z2 :end} :z} c
        lx (-' (inc x2) x1)
        ly (-' (inc y2) y1)
        lz (-' (inc z2) z1)]
    (*' lx ly lz)))

(defn apply-rule [cs rule]
  (let [ins (filter (partial cuboids-intersect? rule) cs)
        outs (filter (complement (partial cuboids-intersect? rule)) cs)]
    (concat
      outs
      (if (:state rule) [rule] [])
      (mapcat #(break-apart-cuboid % rule) ins))))


(comment
  (->> ["on x=-1..0,y=-1..0,z=-1..0"
        "off x=0..0,y=0..0,z=0..0"
        "on x=-53470..21291,y=-120233..-33476,z=-44150..38147"]
       (map parse-line)
       (reduce apply-rule [])
       (map cuboid-area)
       (reduce +))

  (->> (slurp "data/day22.input.txt")
       (string/split-lines)
       (map parse-line)
       (reduce apply-rule [])
       (map cuboid-area)
       (reduce +'))
  
  ())

