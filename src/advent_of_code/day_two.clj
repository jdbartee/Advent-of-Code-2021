(ns advent-of-code.day-two
  (:require [clojure.java.io :as io]
    [clojure.string :as string]))

(defn file-lines [fname]
  (with-open [rdr (io/reader fname)]
    (doall (line-seq rdr))))

(defn starting-position []
  {:h 0
   :d 0
   :a 0})

(defn move-forward [pos amt]
(let [dh amt
      dd (* (:a pos) amt)]
      (as-> pos p
      (update-in p [:h] + dh)
      (update-in p [:d] + dd))))

(defn update-position-with-aim [pos line]
  (let [ upd (if (string? line)
               (cond
                 (string/starts-with? line "forward ")
                 (as-> line l
                   (string/replace l "forward " "")
                   (Integer/parseInt l)
                   (fn [p] (move-forward p l)))
                 (string/starts-with? line "down ")
                 (as-> line l
                   (string/replace l "down " "")
                   (Integer/parseInt l)
                   (fn [p] (update-in p [:a] + l)))
                 (string/starts-with? line "up ")
                 (as-> line l
                   (string/replace l "up " "")
                   (Integer/parseInt l)
                   (fn [p] (update-in p [:a] - l)))
                 :else (fn [p] p))
               (fn [p] p))]
    (upd pos)))

(defn update-position [pos line]
  (let [ upd (if (string? line)
               (cond
                 (string/starts-with? line "forward ")
                 (as-> line l
                   (string/replace l "forward " "")
                   (Integer/parseInt l)
                   (fn [p] (update-in p [:h] + l)))
                 (string/starts-with? line "down ")
                 (as-> line l
                   (string/replace l "down " "")
                   (Integer/parseInt l)
                   (fn [p] (update-in p [:d] + l)))
                 (string/starts-with? line "up ")
                 (as-> line l
                   (string/replace l "up " "")
                   (Integer/parseInt l)
                   (fn [p] (update-in p [:d] - l)))
                 :else (fn [p] p))
               (fn [p] p))]
    (upd pos)))

(defn multiply [pos]
  (* (:h pos) (:d pos)))

(defn task-a [fname]
  (->
   (reduce update-position (starting-position) (file-lines fname))
   (multiply)))

(defn task-b [fname]
  (->
   (reduce update-position-with-aim (starting-position) (file-lines fname))
   (multiply)))

(defn run
  [& args]
  (println "Day 2A")
  (print "Sample:....")
  (println (task-a "data/day2.sample.txt"))
  (print "Real:......")
  (println (task-a "data/day2.input.txt"))
  (println "Day 2B")
  (print "Sample:....")
  (println (task-b "data/day2.sample.txt"))
  (print "Real:......")
(println (task-b "data/day2.input.txt")))