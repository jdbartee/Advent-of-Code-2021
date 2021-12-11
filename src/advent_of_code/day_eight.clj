(ns advent-of-code.day-eight
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.set :as set]))

(defn file-lines [fname]
  (with-open [rdr (io/reader fname)]
    (doall (line-seq rdr))))

(defn split-strs [x]
  (-> (string/trim x)
      (string/split #"\s")
      (->> (map set))))

(defn parse-line [line]
  (-> (zipmap [:pre :output]
              (string/split line #"\|"))
      (update :pre split-strs)
      (update :output split-strs)))

(defn matches [s]
  (-> (count s)
      ((fn [n] (or (= n 2)
                   (= n 3)
                   (= n 4)
                   (= n 7))))))

(defn count-matches-in-output [entry]
  (->> (:output entry)
       (filter matches)
       (count)))

(defn of-length? [l s]
  (= (count s) l))

(defn first-of-length [l coll]
  (first (filter (partial of-length? l) coll)))

(defn solve-fives [decoder samples]
  (let [one (get decoder 1)
        four (get decoder 4)
        four-one (set/difference four one)]
    (apply merge
           (cons decoder
                 (map (fn [s]
                        (cond (set/superset? s one) {3 s}
                              (set/superset? s four-one) {5 s}
                              :else {2 s}))
                      (filter (partial of-length? 5) samples))))))

(defn solve-sixes [decoder samples]
  (let [four (get decoder 4)
        seven (get decoder 7)]
    (apply merge
           (cons decoder
                 (map (fn [s]
                        (cond (set/superset? s four) {9 s}
                              (set/superset? s seven) {0 s}
                              :else {6 s}))
                      (filter (partial of-length? 6) samples))))))

(defn get-decoder [entry]
  (let [samples (:pre entry)]
    (-> {1 (first-of-length 2 samples)
         7 (first-of-length 3 samples)
         4 (first-of-length 4 samples)
         8 (first-of-length 7 samples)}
        (solve-fives samples)
        (solve-sixes samples)
        (set/map-invert))))

(defn process-entry [entry]
  (let [decoder (get-decoder entry)]
    (->> (map #(get decoder %) (:output entry))
         (map str)
         (apply str)
         (#(Integer/parseInt %)))))

(defn task-a [fname]
  (->> (file-lines fname)
       (map parse-line)
       (map count-matches-in-output)
       (reduce +)))

(defn task-b [fname]
  (->> (file-lines fname)
       (map parse-line)
       (map process-entry)
       (reduce +)))

(defn run
  [& args]
  (println "Day 8A")
  (print "Sample:....")
  (println (task-a "data/day8.sample.txt"))
  (print "Real:......")
  (println (task-a "data/day8.input.txt"))
  (println "Day 8B")
  (print "Sample:....")
  (println (task-b "data/day8.sample.txt"))
  (print "Real:......")
  (println (task-b "data/day8.input.txt")))

(comment
  (run))