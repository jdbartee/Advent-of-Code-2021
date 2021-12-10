(ns advent-of-code.day-five
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn file-lines [fname]
  (with-open [rdr (io/reader fname)]
    (doall (line-seq rdr))))

(defn parse-line [line]
  (zipmap [:x1 :y1 :x2 :y2]
          (map #(Integer/parseInt %)
               (rest
                (re-find #"(\d+),(\d+)\s*->\s*(\d+),(\d+)" line)))))

(defn linear? [line]
  (or
   (= (:x1 line) (:x2 line))
   (= (:y1 line) (:y2 line))))

(defn old-expand-line [line]
  (let [x1 (:x1 line)
        x2 (:x2 line)
        y1 (:y1 line)
        y2 (:y2 line)]
    (if (= x1 x2)
      (if (> y1 y2)
        (map (fn [i] {:x x1 :y i}) (range y2 (inc y1)))
        (map (fn [i] {:x x1 :y i}) (range y1 (inc y2))))
      (if (> x1 x2)
        (map (fn [i] {:x i :y y1}) (range x2 (inc x1)))
        (map (fn [i] {:x i :y y1}) (range x1 (inc x2)))))))

(defn my-range [start end]
  (if (> end start)
    (range start (inc end))
    (reverse (range end (inc start)))))

(defn expand-diagonal [line]
  (let [x1 (:x1 line)
        x2 (:x2 line)
        y1 (:y1 line)
        y2 (:y2 line)]
    (let [xs (my-range x1 x2)
          ys (my-range y1 y2)]
      (map (fn [x y] {:x x :y y}) xs ys))))

(defn expand-line [line]
  (if (not (linear? line))
    (expand-diagonal line)
    (let [x1 (:x1 line)
          x2 (:x2 line)
          y1 (:y1 line)
          y2 (:y2 line)]
      (if (= x1 x2)
        (if (> y1 y2)
          (map (fn [i] {:x x1 :y i}) (range y2 (inc y1)))
          (map (fn [i] {:x x1 :y i}) (range y1 (inc y2))))
        (if (> x1 x2)
          (map (fn [i] {:x i :y y1}) (range x2 (inc x1)))
          (map (fn [i] {:x i :y y1}) (range x1 (inc x2))))))))

(defn task-a [fname]
  (->> (file-lines fname)
       (map parse-line)
       (filter linear?)
       (map expand-line)
       (flatten)
       (frequencies)
       (map last)
       (filter #(> % 1))
       (count)))

(defn task-b [fname]
  (->> (file-lines fname)
       (map parse-line)
       (map expand-line)
       (flatten)
       (frequencies)
       (map last)
       (filter #(> % 1))
       (count)))

(defn run
  [& args]
  (println "Day 5A")
  (print "Sample:....")
  (println (task-a "data/day5.sample.txt"))
  (print "Real:......")
  (println (task-a "data/day5.input.txt"))
  (println "Day 5B")
  (print "Sample:....")
  (println (task-b "data/day5.sample.txt"))
  (print "Real:......")
  (println (task-b "data/day5.input.txt")))