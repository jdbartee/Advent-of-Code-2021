(ns advent-of-code.day-nine
  (:require [clojure.java.io :as io]))

(defn file-lines [fname]
  (with-open [rdr (io/reader fname)]
    (doall (line-seq rdr))))

(defn parse-line [line]
  (->> (vec line)
       (map #(Integer/parseInt (str %)))
       (vec)))

(defn parse-lines [lines]
  (vec (map parse-line lines)))

(defn get-neighbors [coll2d x y]
  [(get-in coll2d [(dec x) y] 9)
   (get-in coll2d [(inc x) y] 9)
   (get-in coll2d [x (inc y)] 9)
   (get-in coll2d [x (dec y)] 9)])

(defn less-than-each [n coll]
  (every? (partial < n) coll))

(defn is-low? [n coll2d x y]
  (less-than-each n
                  (get-neighbors coll2d x y)))

(defn process-low-spots [coll2d]
  (reduce +
          (flatten
           (map-indexed
            (fn [x coll]
              (map-indexed
               (fn [y v] (if (is-low? v coll2d x y) (inc v) 0))
               coll))
            coll2d))))

(defn find-low-spots [coll2d]
  (filter map?
          (flatten
           (map-indexed
            (fn [x coll]
              (map-indexed
               (fn [y v] (if (is-low? v coll2d x y) {:x x :y y} false))
               coll))
            coll2d))))

(defn fill-basin [coll2d x y]
  (let [v (get-in coll2d [x y] 9)]
    (cond
      (neg? v) coll2d
      (>= v 9) coll2d
      :else (-> (assoc-in coll2d [x y] -1)
                (fill-basin (inc x) y)
                (fill-basin (dec x) y)
                (fill-basin x (inc y))
                (fill-basin x (dec y))))))

(defn size-basin [coll2d x y]
  (count (filter neg? (flatten (fill-basin coll2d x y)))))

(defn process-basins [coll2d]
  (let [low-spots (find-low-spots coll2d)]
    (map (fn [spot] (size-basin coll2d (:x spot) (:y spot))) low-spots)))

(defn score-basins [basins]
  (reduce * 1 (take-last 3 (sort basins))))

(defn task-a [fname]
  (-> (file-lines fname)
      (parse-lines)
      (process-low-spots)))

(defn task-b [fname]
  (-> (file-lines fname)
      (parse-lines)
      (process-basins)
      (score-basins)))

(defn run
  [& args]
  (println "Day 9A")
  (print "Sample:....")
  (println (task-a "data/day9.sample.txt"))
  (print "Real:......")
  (println (task-a "data/day9.input.txt"))
  (println "Day 9B")
  (print "Sample:....")
  (println (task-b "data/day9.sample.txt"))
  (print "Real:......")
  (println (task-b "data/day9.input.txt")))

(comment
  (run))