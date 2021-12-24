(ns advent-of-code.day-twenty-one)

(defn inc-die [val]
  (let [v (mod (inc val) 100)]
    (if (zero? v) 100 v)))

(defn inc-pos [val n]
  (let [p (mod (+ val n) 10)]
    (if (zero? p) 10 p)))

(defn roll [die n]
  (let [{count :count next :next} die
        rolls (iterate inc-die next)]
    {:die {:count (+ count n) :next (nth rolls n)}
     :result (reduce + (take n rolls))}))

(defn move-player [player n]
  (let [{position :position} player
        new-position (inc-pos position n)]
    (-> player
        (assoc-in [:position] new-position)
        (update-in [:score] + new-position))))

(defn make-game [p1 p2]
  {:player1 {:position p1 :score 0}
   :player2 {:position p2 :score 0}
   :die {:count 0 :next 1}})


(defn take-turn [game p]
  (let [{player p die :die} game
        {die :die result :result} (roll die 3)
        player (move-player player result)]
    (-> game
        (assoc-in [p] player)
        (assoc-in [:die] die))))


(defn get-loser [game]
  (let [{{s1 :score} :player1 {s2 :score} :player2} game]
    (cond (>= s1 1000) :player2
          (>= s2 1000) :player1
          :else nil)))


(defn answer [game loser]
  (let [{{s :score} loser {roll-count :count} :die} game]
    (* s roll-count)))


(defn play [game]
  (loop [game game
         order (cycle [:player1 :player2])]
    (if-let [loser (get-loser game)]
      (answer game loser)
      (recur (take-turn game (first order)) (rest order)))))

(play (make-game 4 8))


;; Part 2 required all new code.

(defn all-rolls []
  (frequencies
    (for [x (range 1 4) 
          y (range 1 4) 
          z (range 1 4)] 
      (+ x y z))))

(defn move-d-player [g p n]
  (let [{pos :p score :s} (p g)
        new-pos (inc-pos pos n)
        new-score (+ score new-pos)]
    (if (>= new-score 21)
      {:w p}
      (-> g
          (assoc-in [p :p] new-pos)
          (assoc-in [p :s] new-score)))))

(defn done? [[game _]]
  (contains? game :w))

(defn take-d-turn [gs p]
  (apply (partial merge-with +')
         (concat
           (for [[g gc] (filter (complement done?) gs)
                 [r rc] (all-rolls)]
             {(move-d-player g p r) 
              (*' gc rc)})
           (map (fn [[k v]] {k v}) (filter done? gs)))))

(defn play-d [games]
  (loop [games games
         order (cycle [:p1 :p2])]
    (if (every? :w (keys games))
      games
      (recur (take-d-turn games (first order)) 
             (rest order)))))

(defn make-d-game [p1 p2]
  {{:p1 {:p p1 :s 0}
    :p2 {:p p2 :s 0}} 1})

(->> (make-d-game 10 9)
     play-d
     vals
     (apply max))

