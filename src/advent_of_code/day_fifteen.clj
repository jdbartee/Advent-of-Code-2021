(ns advent-of-code.day-fifteen
  (:require [clojure.java.io :as io])
  (:use [clojure.test]))

(defn file-lines [fname]
  (with-open [rdr (io/reader fname)]
    (doall (line-seq rdr))))

(defn map-line [line]
  (->> (seq line)
       (map str)
       (mapv #(Integer/parseInt %))))

(defn parse-map [lines]
  (let [h (count lines)
        w (count (first lines))
        g (mapv map-line lines)]
    {:grid g :size {:h h :w w}}))

(defn read-map [fname]
  (-> (file-lines fname) parse-map))

(defn grow-value [v i]
  (inc (mod (+ i (dec v)) 9)))

(defn multiply-row [row]
  (vec
    (concat
      row
      (map (partial grow-value 1) row)
      (map (partial grow-value 2) row)
      (map (partial grow-value 3) row)
      (map (partial grow-value 4) row))))


(defn multiply-grid [grid]
  (let [wider (map multiply-row grid)]
    (vec
      (concat
        wider
        (mapv (partial mapv (partial grow-value 1)) wider)
        (mapv (partial mapv (partial grow-value 2)) wider)
        (mapv (partial mapv (partial grow-value 3)) wider)
        (mapv (partial mapv (partial grow-value 4)) wider)))))

(defn multiply-map [m]
  (let [new-grid (multiply-grid (:grid m))
        h (count new-grid)
        w (count (first new-grid))]
    {:grid new-grid :size {:w w :h h}}))

(defn bottom-right [g]
  (let [size (:size g)]
    {:x (dec (:w size)) :y (dec (:h size))}))

(with-test
  (defn get-neighbors [size used point]
    (let [{w :w h :h} size
          points [(update-in point [:x] inc)
                  (update-in point [:x] dec)
                  (update-in point [:y] inc)
                  (update-in point [:y] dec)
                  ]]
      (->> points
           (filter (fn [c] (< -1 (get-in c [:x]) w)))
           (filter (fn [c] (< -1 (get-in c [:y]) h)))
           (remove (set used)))))
  
  (testing "GET-NEIGHBORS"
    (is
      (= (set [{:x 0 :y 1} {:x 1 :y 0} {:x 2 :y 1} {:x 1 :y 2}])
         (set (get-neighbors {:w 10 :h 10} [] {:x 1 :y 1}))))
    (is
      (= (set [{:x 0 :y 1} {:x 1 :y 0}])
         (set (get-neighbors {:w 10 :h 10} [] {:x 0 :y 0}))))
    (is
      (= (set [{:x 9 :y 8}{:x 8 :y 9}])
         (set (get-neighbors {:w 10 :h 10} [] {:x 9 :y 9}))))
    (is 
      (= (set [{:x 0 :y 2} {:x 1 :y 1}])
         (set (get-neighbors {:w 10 :h 10} #{{:x 0 :y 0}} {:x 0 :y 1}))))))

(with-test
  (defn heuristic [end start]
    (let [{start-x :x start-y :y} start
          {end-x :x end-y :y} end]
      (+ (Math/abs (- end-y start-y))
         (Math/abs (- end-x start-x)))))
  
  (testing "HEURISTIC"
    (is (= 18
           (heuristic {:x 0 :y 0} {:x 9 :y 9})))
    (is (= 9
           (heuristic {:x 9 :y 0} {:x 9 :y 9})))
    (is (= 9
           (heuristic {:x 0 :y 9} {:x 9 :y 9})))
    (is (= 0
           (heuristic {:x 9 :y 9} {:x 9 :y 9})))))


(defn route-extend-to [grid end route point]
  (let [heu (heuristic point end)
        w (get-in grid [(:y point) (:x point)] 10000)]
    {:used (conj (:used route) point)
     :position point
     :weight (+ (:weight route) w)
     :heuristic heu}))

(defn route-score [route]
  (+ (:weight route) (:heuristic route)))

(defn sort-filter-routes [visited routes]
  (->> routes
       (filter (complement #(contains? visited (:position %))))
       (sort-by route-score)))

(defn route-extend-from [neigbor-fn extend-to-fn route]
  (->> (neigbor-fn (:used route) (:position route))
       (map (partial extend-to-fn route))))

(defn a-star [m s e]
  (let [initial-routes [{:used #{s} :position s :weight 0 :heuristic (heuristic s e)}]
        neighbor-fn (partial get-neighbors (:size m))
        extend-to-fn (partial route-extend-to (:grid m) e)
        route-extend (partial route-extend-from neighbor-fn extend-to-fn)]
    
    (loop [routes initial-routes
           visited #{}]
      (let [rs (sort-filter-routes visited routes)
            head (first rs)
            tail (rest rs)
            pos (:position head)]
        (if head
          (if (= pos e)
            head
            (recur (concat tail (route-extend head)) (conj visited pos)))
          false)))))

(defn task-a [fname]
  (let [grid (read-map fname)]
    (a-star grid {:x 0 :y 0} (bottom-right grid))))

(defn task-b [fname]
  (let [grid (multiply-map (read-map fname))]
    (a-star grid {:x 0 :y 0} (bottom-right grid))))

(comment
  (:weight (task-a "data/day15.sample.txt"))
  (:weight (task-b "data/day15.sample.txt"))
  (:weight (task-a "data/day15.input.txt"))
  (:weight (task-b "data/day15.input.txt"))
  ((:test (meta #'get-neighbors)))
  ((:test (meta #'heuristic))))