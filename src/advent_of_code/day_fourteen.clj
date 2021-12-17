(ns advent-of-code.day-fourteen
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn file-lines [fname]
  (with-open [rdr (io/reader fname)]
    (doall (line-seq rdr))))

(defn expand-component [m curr prev]
  (if prev
    (str (get-in m [(str prev curr)] "") curr) 
    (str curr)))

(defn expand-component-m [m p]
  (let [[pair count] p]
    (if (contains? m pair)
      (let [v (get-in m [pair])
            p1 (str (first pair) v)
            p2 (str v (second pair))]
        {p1 count p2 count})
      {pair count})))

(defn step-polymer-m [m poly]
  (apply merge-with +
    (map (partial expand-component-m m) poly)))

(defn step-polymer [m poly]
  (apply str (map (partial expand-component m) poly (cons nil poly))))

(defn parse-rule [line]
  (let [parts (string/split line #" ")
        k (first parts)
        v (last parts)]
    {k v}))

(defn parse-poly [line]
  (as-> line <>
        (str "_" <> "_")
        (partition 2 1 <>)
        (map (partial apply str) <>)
        (frequencies <>)))

(defn score [f]
  (let [v (vals f)]
    (- (apply max v) (apply min v))))

(defn score-m [f]
  (let [v (map #(/ % 2) (vals f))]
    (- (apply max v) (apply min v))))

(defn split-count [p]
  (let [[k c] p
        f (first k)
        s (second k)]
    (if (= f s)
      {f (+ c c)}
      {f c s c})))

(defn frequencies-m [m]
  (dissoc 
    (apply merge-with +
      (map split-count m)) \_))

(defn task-a [fname]
  (let [lines (file-lines fname)
        poly (first lines)
        rules-text (drop 2 lines)
        rules (apply merge (map parse-rule rules-text))]
    (->
        (nth (iterate (partial step-polymer rules) poly) 10)
        (frequencies)
        (score))))

(defn task-b [fname]
  (let [lines (file-lines fname)
        poly (parse-poly (first lines))
        rules-text (drop 2 lines)
        rules (apply merge (map parse-rule rules-text))]
    (->
        (nth (iterate (partial step-polymer-m rules) poly) 40)
        (frequencies-m)
        (score-m))))


(task-b "data/day14.input.txt") 