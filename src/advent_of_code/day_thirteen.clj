(ns advent-of-code.day-thirteen
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))


(defn file-lines [fname]
  (with-open [rdr (io/reader fname)]
    (doall (line-seq rdr))))


(defmacro debug [b]
  `(let [r# ~b]
     (prn r#)
     r#))


(defn parse-points [points]
  (set 
    (map (fn [line]
           (->> (string/split line #",")
                (map #(Integer/parseInt %))
                (zipmap [:x :y])))
         points)))

(defn fold-value [v p]
  (if (< p v)
    p
    (- (* 2 v) p)))

(defn apply-instruction-to-point [i p]
  (let [a (:axis i)
        v (:value i)]
    (cond 
      (= a "x") (update-in p [:x] (partial fold-value v))
      (= a "y") (update-in p [:y] (partial fold-value v))
      :else p)))

(defn apply-instruction [i ps]
  (set
    (map (partial apply-instruction-to-point i) ps)))

(defn parse-instructions [instructions]
  (->> (map (fn [line]
              (rest
                (re-find #"fold along ([x|y])=(\d+)" line))
              ) instructions)
       (filter seq)
       (map (partial zipmap [:axis :value]))
       (map (fn [m] (update-in m [:value] #(Integer/parseInt %))))))

(defn draw-points [ps]
  (let [xmax (apply max (map :x ps))
        ymax (apply max (map :y ps))
        xs (range 0 (inc xmax))
        ys (range 0 (inc ymax))]
    (println
      (string/join "\n"
                   (map (fn [y]
                          (string/join " "(mapv (fn [x] (if (contains? ps {:x x :y y}) "#" ".")) xs))) ys)))))

(defn task-a [fname]
  (let [[points instructions] (split-with seq (file-lines fname))
        ps (parse-points points)
        is (parse-instructions instructions)]
    (-> (apply-instruction (first is) ps)    
        (count))))

(defn task-b [fname]
  (let [[points instructions] (split-with seq (file-lines fname))
        ps (parse-points points)
        is (parse-instructions instructions)]
    (-> (reduce #(apply-instruction %2 %1) ps is)
        (draw-points))))

(comment
  (task-b "data/day13.input.txt"))