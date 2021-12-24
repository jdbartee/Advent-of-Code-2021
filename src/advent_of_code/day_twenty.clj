(ns advent-of-code.day-twenty 
  (:require
    [clojure.string :as string]))

(defn powers-of-two []
  (map #(Math/pow 2 %) (range 100)))

(defn boolseq->num [coll]
  (->> (reverse coll)
       (map #(if %2 %1 0) (powers-of-two))
       (reduce +)
       long))

(defn parse-pattern [line]
  (mapv (partial = \#) line))

(defn parse-image [lines]
  {:data (mapv parse-pattern lines)
   :default false})

(defn parse-data [lines]
  {:pattern (parse-pattern (first lines))
   :image   (parse-image   (drop 2 lines))})


(defn value-at-index [image idx]
  (boolseq->num
    (let [[y x] idx
          {data :data default :default} image]
      [(get-in data [(dec y) (dec x)] default)
       (get-in data [(dec y) x] default)
       (get-in data [(dec y) (inc x)] default)
       (get-in data [y (dec x)] default)
       (get-in data [y x] default)
       (get-in data [y (inc x)] default)
       (get-in data [(inc y) (dec x)] default)
       (get-in data [(inc y) x] default)
       (get-in data [(inc y) (inc x)] default)])))

(defn row-string [row] 
  (->> (map #(if % \# \.) row)
       string/join))

(defn print-image [image]
  (println {:default (:default image)})
  (->> (map row-string (:data image))
       (string/join "\n")
       println))

(defn apply-pattern [pattern image]
  (let [{data :data default :default} image]
      (let [new-default (get-in pattern [(boolseq->num (repeat 9 default))] default)
            new-data (let [height (count data)
                width (count (first data))
                value (partial value-at-index image)]
            (for [y (range -1 (+ height 2)) x (range -1 (+ width 2))]
              (get-in pattern [(value [y x])] default)))]
        {:data new-data 
         :default new-default})))

(defn enhance [data]
  (let [{pattern :pattern image :image} data 
        {image-data :data default :default} image
        value (partial value-at-index image)
        new-default (get-in pattern [(boolseq->num (repeat 9 default))] "X")
        new-data (let [height (count image-data)
                       width (count (first image-data))]
                   (mapv (fn [y]
                          (mapv (fn [x]
                                 (get-in pattern [(value [y x])] "X"))
                               (range -1 (+ width 2)))) 
                        (range -1 (+ height 2))))]
    {:pattern pattern :image {:data new-data :default new-default}}))

(->>
  (slurp "data/day20.input.txt")
  string/split-lines
  parse-data
  (iterate enhance)
  (#(nth % 50))
  :image
  :data
  flatten
  (filter identity)
  count)


