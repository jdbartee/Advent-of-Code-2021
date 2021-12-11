(ns advent-of-code.day-seven
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))


(defn file-lines [fname]
  (with-open [rdr (io/reader fname)]
    (doall (line-seq rdr))))

(defn parse-line [line]
  (->> (string/split line #",")
       (map #(Integer/parseInt %))
       (frequencies)))

(defn test-range [crabs]
  (let [ks (keys crabs)
        lower (apply min ks)
        upper (apply max ks)]
    (range lower (inc upper))))

(defn triangle-number [n]
  (/ (* n (inc n)) 2))

(defn cost-to-move-to [crabs target]
  (let [cost-one-set (fn [e]
                       (* (val e) (Math/abs (- target (key e)))))]
    (reduce + (map cost-one-set crabs))))

(defn cost-to-move-to-b [crabs target]
  (let [cost-one-set (fn [e]
                       (* (val e) (triangle-number (Math/abs (- target (key e))))))]
    (reduce + (map cost-one-set crabs))))

(defn task-a [fname]
  (let [crabs (-> (file-lines fname)
                  (first)
                  (parse-line))]
    (apply min (map (partial cost-to-move-to crabs)
                    (test-range crabs)))))

(defn task-b [fname]
  (let [crabs (-> (file-lines fname)
                  (first)
                  (parse-line))]
    (apply min (map (partial cost-to-move-to-b crabs)
                    (test-range crabs)))))

(defn run
  [& args]
  (println "Day 7A")
  (print "Sample:....")
  (println (task-a "data/day7.sample.txt"))
  (print "Real:......")
  (println (task-a "data/day7.input.txt"))
  (println "Day 7B")
  (print "Sample:....")
  (println (task-b "data/day7.sample.txt"))
  (print "Real:......")
  (println (task-b "data/day7.input.txt")))

(comment
  (run)
)