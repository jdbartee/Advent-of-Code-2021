(ns advent-of-code.day-eleven
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))


(defn file-lines [fname]
  (with-open [rdr (io/reader fname)]
    (doall (line-seq rdr))))

(def flashes (atom 0))

(defn reset-flashes []
  (swap! flashes (constantly 0)))

(defn increment-flashes []
  (swap! flashes inc))

(defn increase-energy [octopuses x y]
  (if (nil? (get-in octopuses [x y] nil))
    octopuses
    (let [oct (update-in octopuses [x y] inc)]
      (if (= (get-in oct [x y]) 10)
        (-> oct
            (increase-energy (inc x) (inc y))
            (increase-energy (inc x) (dec y))
            (increase-energy (dec x) (inc y))
            (increase-energy (dec x) (dec y))
            (increase-energy (inc x) y)
            (increase-energy (dec x) y)
            (increase-energy x (inc y))
            (increase-energy x (dec y)))
        oct))))

(defn increase-all-energy [octopuses]
  (reduce
   (fn [o p]
     (increase-energy o (:x p) (:y p)))
   octopuses
   (for [x (range (count octopuses))
         y (range (count (first octopuses)))]
     {:x x :y y})))

(defn process-flashes [octopuses]
  (mapv
   (fn [coll]
     (mapv (fn [o]
             (if (> o 9)
               (do
                 (increment-flashes)
                 0)
               o)) coll))
   octopuses))

(defn process-step [octopuses]
  (-> (increase-all-energy octopuses)
      (process-flashes)))

(defn count-flashes [steps octopuses]
  (if (zero? steps)
    @flashes
    (count-flashes (dec steps) (process-step octopuses))))

(defn process-until-all-flash [steps octopuses]
  (if (zero? (reduce + (flatten octopuses)))
    steps
    (process-until-all-flash (inc steps) (process-step octopuses))))

(defn parse-line [line]
  (->> (vec line)
       (map str)
       (map #(Integer/parseInt %))
       (vec)))

(defn task-a [fname]
  (reset-flashes)
  (->> (file-lines fname)
       (mapv parse-line)
       (count-flashes 100)))

(defn task-b [fname]
  (reset-flashes)
  (->> (file-lines fname)
       (mapv parse-line)
       (process-until-all-flash 0)))

(defn run
  [& args]
  (println "Day 11A")
  (print "Sample:....")
  (println (task-a "data/day11.sample.txt"))
  (print "Real:......")
  (println (task-a "data/day11.input.txt"))
  (println "Day 11B")
  (print "Sample:....")
  (println (task-b "data/day11.sample.txt"))
  (print "Real:......")
  (println (task-b "data/day11.input.txt")))