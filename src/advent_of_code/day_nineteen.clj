(ns advent-of-code.day-nineteen 
  (:require
    [clojure.string :as string]
    [clojure.set :as set]))

(defn rot-fns []
  [(fn [p] (let [[a b c] p] [a b c]))
   (fn [p] (let [[a b c] p] [b (- a) c]))
   (fn [p] (let [[a b c] p] [(- a) (- b) c]))
   (fn [p] (let [[a b c] p] [(- b) a c]))
   (fn [p] (let [[a b c] p] [a (- c) b]))
   (fn [p] (let [[a b c] p] [(- c) (- a) b]))
   (fn [p] (let [[a b c] p] [c a b]))
   (fn [p] (let [[a b c] p] [(- a) c b]))
   (fn [p] (let [[a b c] p] [a (- b) (- c)]))
   (fn [p] (let [[a b c] p] [(- b) (- a) (- c)]))
   (fn [p] (let [[a b c] p] [(- a) b (- c)]))
   (fn [p] (let [[a b c] p] [b a (- c)]))
   (fn [p] (let [[a b c] p] [a c (- b)]))
   (fn [p] (let [[a b c] p] [c (- a) (- b)]))
   (fn [p] (let [[a b c] p] [(- a) (- c) (- b)]))
   (fn [p] (let [[a b c] p] [(- c) a (- b)]))
   (fn [p] (let [[a b c] p] [b c a]))
   (fn [p] (let [[a b c] p] [c (- b) a]))
   (fn [p] (let [[a b c] p] [(- b) (- c) a]))
   (fn [p] (let [[a b c] p] [(- c) b a]))
   (fn [p] (let [[a b c] p] [b (- c) (- a)]))
   (fn [p] (let [[a b c] p] [(- c) (- b) (- a)]))
   (fn [p] (let [[a b c] p] [(- b) c (- a)]))
   (fn [p] (let [[a b c] p] [c b (- a)]))])



(defn parse-point [line]
  (as-> line <>
        (string/split <> #",")
        (mapv #(Integer/parseInt %) <>)))

(defn read-data [fname]
  (as-> fname <>
        (slurp <>)
        (string/split <> #"\n\n")
        (map string/split-lines <>)
        (map rest <>)
        (map #(map parse-point %) <>)
        (let [stable (first <>)
              unknown (rest <>)]
          {:stable (list stable)
           :offsets (list [0 0 0])
           :unknown unknown})))

(defn offset-scanner [offset points]
  (map #(mapv + % offset) points))


(defn compare-sets [ks us]
  (let [offsets (->> (for [k ks u us] (mapv - k u))
                     frequencies
                     (filter #(>= (val %) 12))
                     (sort-by val)
                     keys)]
    (->> (map (fn [offset]
                (let [adjusted-points (offset-scanner offset us)]
                  {:offset offset 
                   :points adjusted-points}))
              offsets)
         (filter (fn [points]
                   (>= (count (set/intersection (set (:points points)) (set ks))) 12)))
         first)))


(defn update-data [data]
  (reduce (fn [s u]
            (if-let [new-val 
                     (->> (for [ks (:stable s) us (map #(map % u) (rot-fns))]
                            (compare-sets ks us))
                          (filter identity)
                          first)]
              (-> s 
                  (update-in [:stable] concat [(:points new-val)])
                  (update-in [:offsets] concat [(:offset new-val)]))
              (update-in s [:unknown] concat [u]))) 
          {:stable (:stable data) 
           :unknown []
           :offsets (:offsets data)} 
          (:unknown data)))

(defn count-unique [data]
  (-> (apply concat data)
      frequencies
      keys
      count))

(defn distances [data]
  (prn (:offsets data))
  (apply max
         (for [o1 (:offsets data) o2 (:offsets data)]
           (reduce + (map #(Math/abs (- %1 %2)) o1 o2)))))

(defn solve [data]
  (let [next (update-data data)]
    (if (empty? (:unknown next))
      {:count (count-unique (:stable next)) :size (distances next)}
      (if (= (count (:unknown data)) (count (:unknown next)))
        "Error: Not Found!"
        (recur next)))))

(comment
  (prn)
  (-> (read-data "data/day19.input.txt")
      solve)
  
  
  )