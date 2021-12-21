(ns advent-of-code.day-eighteen 
  (:require
    [clojure.string :as string]))


(defn make-pair [v]
  (if (coll? v)
    (let [[l r] v]
      {:l (make-pair l) :r (make-pair r)})
    v))

(defn parse-number [line]
  (make-pair (read-string line)))

(defn max-indexes [n]
  (let [extend-fn 
        (fn [ks]
          (if (empty? ks)
            [[:l] [:r]]
            (mapcat (fn [k] [(conj k :l) (conj k :r)]) ks)))]
    (nth (iterate extend-fn []) n)))

(defn recur-indexs [prefix depth]
  (if (pos? depth)
    (concat
      [(concat prefix [:l])] 
       (recur-indexs (concat prefix [:l]) (dec depth))
       [(concat prefix [:r])]
       (recur-indexs (concat prefix [:r]) (dec depth)))
      []))

(defn all-indexes [n]
  (recur-indexs [] n))

(all-indexes 4)


(all-indexes 4)

(defn descend-tree [tree dir idx]
  (let [curr (get-in tree (vec idx))]
    (cond
      (int? curr) (vec idx)
      (map? curr) (recur tree dir (concat (vec idx) [dir]))
      :else nil)))

(defn leftmost-sibling-index [tree idx]
  (cond
    (empty? idx) nil
    (= :l (last idx)) (leftmost-sibling-index tree (butlast idx))
    :else (descend-tree tree :r (concat (butlast idx) [:l]))))

(defn rightmost-sibling-index [tree idx]
  (cond 
    (empty? idx) nil
    (= :r (last idx)) (rightmost-sibling-index tree (butlast idx))
    :else (descend-tree tree :l (concat (butlast idx) [:r]))))

(defn should-split? [v]
  (if (integer? v)
    (>= v 10)
    false))

(defn split-num [v]
  (let [l (int (Math/floor (/ v 2)))
        r (int (Math/ceil (/ v 2)))]
    {:l l :r r}))


(defn snailfish-reduce [a]
  (if-let [index-to-exp (first (filter #(map? (get-in a %)) (max-indexes 4)))]
    (let [left-value (get-in a (concat index-to-exp [:l]))
          right-value (get-in a (concat index-to-exp [:r]))
          lms (leftmost-sibling-index a index-to-exp)
          rms (rightmost-sibling-index a index-to-exp)]
      (prn "Explode at:" index-to-exp)
      (as-> a <>
            (if lms (update-in <> lms + left-value) <>)
            (if rms (update-in <> rms + right-value) <>)
            (assoc-in <> index-to-exp 0)
            (recur <>)))
    (if-let [index-to-split (first (filter #(should-split? (get-in a %)) (all-indexes 4)))]
      (do(prn "Split at:" index-to-split)
      (as-> a <>
            (update-in <> index-to-split split-num)
            (recur <>)))
      a)))

(defn snailfish-add [a b]
  (snailfish-reduce {:l a :r b}))

(defn magnitude [v]
  (if (integer? v)
    v
    (+ 
      (* 3 (magnitude (:l v)))
      (* 2 (magnitude (:r v))))))

(as-> (slurp "data/day18.input.txt") <>
      (string/split-lines <>)
      (map parse-number <>)
      (reduce snailfish-add <>)
      (magnitude <>))

(as-> (slurp "data/day18.input.txt") <>
      (string/split-lines <>)
      (map parse-number <>)
      (for [a <> b <>]
        (if (= a b)
          0
          (magnitude (snailfish-add a b))))
      (apply max <>))


(prn "")
(comment
  (snailfish-reduce {:l {:l 10 :r 10} :r  {:l 10 :r 10}})
  (snailfish-add 
    (make-pair [[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]])
    (make-pair [7,[[[3,7],[4,3]],[[6,3],[8,8]]]]))
  )

