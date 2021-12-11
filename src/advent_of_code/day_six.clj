(ns advent-of-code.day-six
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn file-lines [fname]
  (with-open [rdr (io/reader fname)]
    (doall (line-seq rdr))))

(defn parse-line [line]
  (->> (string/split line #",")
       (map #(Integer/parseInt %))
       (frequencies)))

(defn age-school [school]
  (let [u (fn [s k]
              (let [plus (fnil + 0)
                    v (get school k 0)]
                (if (zero? k)
                  (-> (update s 6 plus v)
                      (update , 8 plus v))
                  (update s (dec k) plus v))))]
      (reduce u {} (range 0 9))))

(defn simulate-school [school & {:keys [days]
                                 :or {days 0}}]
  (loop [school school
         days days]
    (if (pos? days)
      (recur
       (age-school school) (dec days))
      school)))

(defn count-school [school]
  (->> (map val school)
       (reduce +)))

(defn task-a [fname]
  (-> (file-lines fname)
      (first)
      (parse-line)
      (simulate-school :days 80)
      (count-school)))

(defn task-b [fname]
  (-> (file-lines fname)
      (first)
      (parse-line)
      (simulate-school :days 256)
      (count-school)))

(defn run
  [& args]
  (println "Day 6A")
  (print "Sample:....")
  (println (task-a "data/day6.sample.txt"))
  (print "Real:......")
  (println (task-a "data/day6.input.txt"))
  (println "Day 6B")
  (print "Sample:....")
  (println (task-b "data/day6.sample.txt"))
  (print "Real:......")
  (println (task-b "data/day6.input.txt")))