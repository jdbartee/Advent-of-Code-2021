(ns advent-of-code.day-four
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn file-lines [fname]
  (with-open [rdr (io/reader fname)]
    (doall (line-seq rdr))))

(defn starting-data []
  {:calls []
   :boards []})

(defn parse-int [s]
  (Integer/parseInt s))

(defn add-calls-to-data [data line]
  (as-> line v
    (string/split v #",")
    (map parse-int v)
    (assoc-in data [:calls] (vec v))))

(defn parse-board [lines]
  (->> lines
      (map #(string/trim %))
      (map #(string/split % #"\s+"))
      (map #(map parse-int %))))

(defn add-boards-to-data [data lines]
  (let [board-data
        (->> lines
             (partition-by (partial not= ""))
             (filter #(not-empty (rest %))))]
    (as-> (map parse-board board-data) boards
        (assoc-in data [:boards] (vec (map vec boards))))))

(defn apply-call-to-board [call board]
  (map (fn [y] (map (fn [x] (if (= call x) false x)) y)) board))

(defn check-row [row]
  (nil? (some #(not (false? %)) row)))

(defn check-rows [board]
  (boolean (some true? (map check-row board))))

(defn check-columns [board]
  (check-rows (apply mapv vector board)))

(defn check-board [board]
  (or (check-rows board) (check-columns board)))

(defn score-board [i call board]
  `(~i ~(* call (reduce + 0 (map (fn [x] (if x x 0)) (flatten board))))))

(defn process-board [calls board]
  (loop [i 0
         cs calls
         b board]
    (if-let [call (first cs)]
      (let [new-board (apply-call-to-board call b)]
        (if (check-board new-board)
          (score-board i call new-board)
          (recur (inc i) (rest cs) new-board)))
      `(false 0))))

(defn score-data [data]
  (map (partial process-board (:calls data)) (:boards data)))

(defn final-value [scores]
  (last (first (sort-by first scores))))

(defn final-value-b [scores]
  (last (last (sort-by first scores))))

(defn parse-data [lines]
  (let [calls (first lines)
        data (rest (rest lines))]
    (-> (starting-data)
        (add-calls-to-data calls)
        (add-boards-to-data data))))

(defn task-a [fname]
  (-> (file-lines fname)
      (parse-data)
      (score-data)
      (final-value)))

(defn task-b [fname]
  (-> (file-lines fname)
      (parse-data)
      (score-data)
      (final-value-b)))

(defn run
  [& args]
  (println "Day 4A")
  (print "Sample:....")
  (println (task-a "data/day4.sample.txt"))
  (print "Real:......")
  (println (task-a "data/day4.input.txt"))
  (println "Day 4B")
  (print "Sample:....")
  (println (task-b "data/day4.sample.txt"))
  (print "Real:......")
  (println (task-b "data/day4.input.txt")))