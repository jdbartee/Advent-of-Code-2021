(ns advent-of-code.day-ten
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn file-lines [fname]
  (with-open [rdr (io/reader fname)]
    (doall (line-seq rdr))))

(def pairs { \} \{  \) \(  \> \< \] \[ })
(def points { \) 3 \] 57 \} 1197 \> 25137 \( 1 \[ 2 \{ 3 \< 4 })

(defn parse-line [line]
  (vec line))

(defn score-line-errors [stack line]
  (if (empty? line)
    0
    (let [s (first line)
          needed (get pairs s nil)]
      (if (nil? needed)
        (recur (cons s stack) (rest line))
        (if (= needed (first stack))
          (recur (rest stack) (rest line))
          (get points s 0))))))

(defn score-remaining-stack [score stack]
  (if (empty? stack)
    score
    (recur (+ (* 5 score) (get points (first stack) 0)) (rest stack))))

(defn score-line-completion [stack line]
  (if (empty? line)
    (score-remaining-stack 0 stack)
    (let [s (first line)
          needed (get pairs s nil)]
      (if (nil? needed)
        (recur (cons s stack) (rest line))
        (if (= needed (first stack))
          (recur (rest stack) (rest line))
          0)))))

(defn median [coll]
  (if (= 1 (count coll))
    (first coll)
    (recur (butlast (rest coll)))))

(defn task-a [fname]
  (->> (file-lines fname)
       (map parse-line)
       (map (partial score-line-errors []))
       (reduce +)))

(defn task-b [fname]
  (->> (file-lines fname)
       (map parse-line)
       (map (partial score-line-completion []))
       (filter pos?)
       (sort)
       (median)))

(defn run
  [& args]
  (println "Day 10A")
  (print "Sample:....")
  (println (task-a "data/day10.sample.txt"))
  (print "Real:......")
  (println (task-a "data/day10.input.txt"))
  (println "Day 10B")
  (print "Sample:....")
  (println (task-b "data/day10.sample.txt"))
  (print "Real:......")
  (println (task-b "data/day10.input.txt")))