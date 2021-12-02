(ns advent-of-code.day-one)

(defn file-lines [fname]
  (with-open [rdr (clojure.java.io/reader fname)]
    (doall (line-seq rdr))))

(defn did-increase? [[prev curr]]
  (if (> curr prev) 1 0))

(defn sum3 [[a b c]]
  (+ a b c))

(defn count-increases [coll]
  (as-> coll a
    (partition 2 1 a)
    (map did-increase? a)
    (reduce + a)))

(defn task-a [fname]
  (as-> (file-lines fname) a
    (map #(Integer/parseInt %) a)
    (count-increases a)))

(defn task-b [fname]
  (as-> (file-lines fname) a
    (map #(Integer/parseInt %) a)
       ; Partition in groups of 3 and get sums of the groups
    (partition 3 1 a)
    (map sum3 a)
    (count-increases a)))

(defn day-one
  [& args]
  (println "Day 1A")
  (print "Sample:....")
  (println (task-a "data/day1.sample.txt"))
  (print "Real:......")
  (println (task-a "data/day1.input.txt"))
  (println "Day 1B")
  (print "Sample:....")
  (println (task-b "data/day1.sample.txt"))
  (print "Real:......")
  (println (task-b "data/day1.input.txt")))
