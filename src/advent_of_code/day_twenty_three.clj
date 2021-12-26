(ns advent-of-code.day-twenty-three 
  (:require
    [clojure.string :as string]))

(def room-depth 2)

(defn route-blocked [hallway r idx]
  (let [inflection-pt (case r :a 2 :b 3 :c 4 :d 5)
        range-to-check (cond 
                         (= idx inflection-pt) [inflection-pt]
                         (> idx inflection-pt) (range inflection-pt (inc idx))
                         (< idx inflection-pt) (range idx inflection-pt))]
    (some #(get-in hallway [%]) range-to-check)))

(defn get-cost [t]
  (case t
    :A 1
    :B 10
    :C 100
    :D 1000))

(defn get-target [t]
  (case t
    :A :a
    :B :b
    :C :c
    :D :d))

(defn get-str [t]
  (case t
    :A "A"
    :B "B"
    :C "C"
    :D "D"
    "."))

(defn room->hallway [r idx]
  (case r
    :a (case idx 0 3, 1 2, 2 2, 3 4, 4 6, 5 8, 6 9)
    :b (case idx 0 5, 1 4, 2 2, 3 2, 4 4, 5 6, 6 7)
    :c (case idx 0 7, 1 6, 2 4, 3 2, 4 2, 5 4, 6 5)
    :d (case idx 0 9, 1 8, 2 6, 3 4, 4 2, 5 2, 6 3)))

(defn hallway->room [idx r]
  (room->hallway r idx))

(defn room->room [r1 r2]
  (case r1
    :a (case r2 :a 0 :b 4 :c 6 :d 8)
    :b (case r2 :a 4 :b 0 :c 4 :d 6)
    :c (case r2 :a 6 :b 4 :c 0 :d 4)
    :d (case r2 :a 8 :b 6 :c 4 :d 0)))

(defn get-heuristic [state]
  (let[ heuristic
       (reduce 
         +
         (flatten
           (concat
             [(:score state)]
             (for [idx (range 0 7)]
               (if-let [v (get-in (:hallway state) [idx])]
                 (* (get-cost v) (hallway->room idx (get-target v)))
                 0))
             (for [room [:a :b :c :d]]
               (map #(* 
                       (get-cost %) 
                       (room->room room (get-target %))) 
                    (get-in state [:rooms room] []))))))]
    (if (< heuristic (:heuristic state))
      (throw (Exception. "Heuristic shouldn't shrink")))
    heuristic))

(defn do-room->hallway [state room idx]
  (let [hallway (:hallway state)
        r (get-in state [:rooms room])
        v (first r)]
    (if (and (some? v)
             (not (and (= room (get-target v))
                       (every? (partial = v) r)))
             (not (route-blocked hallway room idx)))
      (let [distance (room->hallway room idx)
            extra (- room-depth (count r))]
        (-> state
            (update-in [:rooms room] rest)
            (assoc-in [:hallway idx] v)
            (update-in [:score] + (* (get-cost v) 
                                     (+ extra distance)))
            (update-in [:history] conj {:from room :to idx})
            ((fn [n] (assoc-in n [:heuristic] (get-heuristic n))))))
      nil)))

(defn do-hallway->room [state idx room]
  (let [hallway (:hallway state)
        v (get-in hallway [idx])
        r (get-in state [:rooms room])]
    (if (and (some? v)
             (> room-depth (count r))
             (or (nil? (first r))
                 (= v (first r)))
             (not (route-blocked (assoc-in hallway [idx] nil) room idx)))
      (let [distance (hallway->room idx room)
            extra (- room-depth (inc (count r)))]
        (-> state
            (assoc-in [:hallway idx] nil)
            (assoc-in [:rooms room] (concat [v] r))
            (update-in [:score] + (* (get-cost v)
                                     (+ extra distance)))
            (update-in [:history] conj {:from idx :to room})
            ((fn [n] (assoc-in n [:heuristic] (get-heuristic n))))))
      nil)))

(defn finished? [state]
  (= (:heuristic state) (:score state)))


(defn get-descr [state]
  (string/join
  (concat
    (map get-str (:hallway state))
    ":a"
    (map get-str (get-in state [:rooms :a]))
    ":b"
    (map get-str (get-in state [:rooms :b]))
    ":c"
    (map get-str (get-in state [:rooms :c]))
    ":d"
    (map get-str (get-in state [:rooms :d])))))

(defn solve [state]
  (loop [states [state]
         visited #{}]
    (let [ss (sort-by :heuristic states)
          curr (first ss)
          remaining (rest ss)
          descr (get-descr curr)]
      (if (contains? visited descr)
        (recur remaining visited)
        (do
          (prn {:score (:score curr) :heuristic (:heuristic curr)})
          (if (or (nil? curr)
                  (finished? curr))
            curr
            (let [next 
                  (filter some? (concat
                                  remaining
                                  (for [idx (range 0 7)]
                                    (if-let [v (get-in (:hallway curr) [idx])]
                                      (do-hallway->room curr idx (get-target v))
                                      nil))
                                  (for [room [:a :b :c :d] idx (range 0 7)]
                                    (do-room->hallway curr room idx))))]
              (recur next (conj visited descr)))))))))


(def setup
  {:heuristic -1
   :score 0
   :hallway [nil nil nil nil nil nil nil] 
   :rooms {:a [:B :C]
           :b [:A :D]
           :c [:B :D]
           :d [:C :A]}
   :history []})

(def test-setup
  {:heuristic -1
   :score 0
   :hallway [nil nil nil nil nil nil nil] 
   :rooms {:a [:B :A]
           :b [:C :D]
           :c [:B :C]
           :d [:D :A]}
   :history []})

(def setup-b
  {:heuristic -1
   :score 0
   :hallway [nil nil nil nil nil nil nil] 
   :rooms {:a [:B :D :D :C]
           :b [:A :C :B :D]
           :c [:B :B :A :D]
           :d [:C :A :C :A]}
   :history []})

(get-heuristic test-setup)
(get-descr test-setup)
#_(-> (solve setup))

(with-redefs [room-depth 4]
             (solve setup-b))


