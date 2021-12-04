(ns advent-of-code.day-three
  (:require [clojure.java.io :as io]))

 (defn file-lines [fname]
   (with-open [rdr (io/reader fname)]
     (doall (line-seq rdr))))


(defn task-a-starting-state []
  {:total 0
   :counts []})

(defn line->list-of-int [line]
  (vec (->> line
            (seq)
            (map str)
            (map #(Integer/parseInt %)))))

(defn vec->value [v]
  (:r (reduce (fn [s d]
                (-> s
                    (update-in [:r] + (* (if d 1 0) (:b s)))
                    (update-in [:b] * 2)))
              {:b 1 :r 0} v)))

(defn get-most-common-at [vecs index]
  (let [threshold (/ (count vecs) 2)
        sum (reduce + (map #(get % index) vecs))]
    (if (<= threshold sum) 1 0)))
    
 (defn get-least-common-at [vecs index]
   (let [threshold (/ (count vecs) 2)
         sum (reduce + (map #(get % index) vecs))]
     (if (> threshold sum) 1 0)))

(defn index-is-digit [vec index digit]
  (= digit (get vec index)))

 (defn reduce-oxygen-vecs [vecs index]
   (let [most-common (get-most-common-at vecs index)]
     (filter #(index-is-digit % index most-common) vecs)))

 (defn reduce-co2-vecs [vecs index]
   (let [least-common (get-least-common-at vecs index)]
     (filter #(index-is-digit % index least-common) vecs)))

(defn find-oxygen-value [vecs index]
  (if (= 1 (count vecs))
    (vec->value (map #(= 1 %) (reverse (nth vecs 0))))
    (-> (reduce-oxygen-vecs vecs index)
        (find-oxygen-value (+ 1 index)))))

 (defn find-co2-value [vecs index]
 (if (= 1 (count vecs))
   (vec->value (map #(= 1 %) (reverse (nth vecs 0))))
   (-> (reduce-co2-vecs vecs index)
       (find-co2-value (+ 1 index)))))

(defn task-a-next-state [state line]
  (if (= (:total state) 0)
    (as-> state s
      (update-in s [:total] (constantly 1))
      (update-in s [:counts] (constantly (line->list-of-int line))))
    (as-> state s
      (update-in s [:total] + 1)
      (update-in s [:counts] #(mapv + (line->list-of-int line) %)))))

(defn gamma-rate [state]
  (let [threshold (/ (:total state) 2)
        v (reverse (:counts state))]
        (->> v
             (map #(>= % threshold))
             (vec->value))))

(defn epsilon-rate [state]
  (let [threshold (/ (:total state) 2)
        v (reverse (:counts state))]
    (->> v
         (map #(< % threshold))
         (vec->value))))

(defn task-a [fname]
  (let [state (->> (file-lines fname)
                   (reduce task-a-next-state (task-a-starting-state)))]
    (* (gamma-rate state) (epsilon-rate state))))

(defn task-b [fname]
  (let [lines (file-lines fname)
        vecs (map line->list-of-int lines)
        oxygen-value (find-oxygen-value vecs 0)
        co2-value (find-co2-value vecs 0)]
    (* oxygen-value co2-value)))


(defn run
  [& args]
  (println "Day 3A")
  (print "Sample:....")
  (println (task-a "data/day3.sample.txt"))
  (print "Real:......")
  (println (task-a "data/day3.input.txt"))
  (println "Day 3B")
  (print "Sample:....")
  (println (task-b "data/day3.sample.txt"))
  (print "Real:......")
  (println (task-b "data/day3.input.txt")))