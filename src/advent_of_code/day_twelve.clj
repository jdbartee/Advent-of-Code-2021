(ns advent-of-code.day-twelve
  (:require
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as string]))

(defn file-lines [fname]
  (with-open [rdr (io/reader fname)]
    (doall (line-seq rdr))))

(defn parse-line [line]
  (let [parts (string/split line #"-" 2)]
        {(first parts) #{(last parts)} 
         (last parts) #{(first parts)}}))

(defn merge-graphs [a b]
  (merge-with set/union a b))

(defn navigate-graph [g c v r x]
  (let [route (conj r c)
        visit (conj v c)
        next-node (fn [b] (fn [n] (navigate-graph g n visit route b)))]
    (cond 
      (= c "end") [route]
      (and (Character/isLowerCase (first c))
           (contains? v c)) 
              (if (and x (not= c "start"))
                (reduce concat (mapv (next-node false) (get g c [])))
                [])
      :else (reduce concat (mapv (next-node x) (get g c []))))))


(defn task-a [fname]
  (->> (file-lines fname)
       (map parse-line)
       (reduce merge-graphs {})
       (#(navigate-graph % "start" #{} [] false))
       (count)))


(defn task-b [fname]
  (->> (file-lines fname)
       (map parse-line)
       (reduce merge-graphs {})
       (#(navigate-graph % "start" #{} [] true))
       (count)))

(defn run [& args]
  (println "Day 12A")
  (print "Sample:....")
  (println (task-a "data/day12.sample.txt"))
  (print "Real:......")
  (println (task-a "data/day12.input.txt"))
  (println "Day 12B")
  (print "Sample:....")
  (println (task-b "data/day12.sample.txt"))
  (print "Real:......")
  (println (task-b "data/day12.input.txt")))

(comment
  (run))