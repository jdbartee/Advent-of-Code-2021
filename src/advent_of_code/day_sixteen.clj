(ns advent-of-code.day-sixteen)

(defn hexchar->boolseq [c]
  (let [T true
        F false]
    (case c
      \0 [F F F F]
      \1 [F F F T]
      \2 [F F T F]
      \3 [F F T T]
      \4 [F T F F]
      \5 [F T F T]
      \6 [F T T F]
      \7 [F T T T]
      \8 [T F F F]
      \9 [T F F T]
      \A [T F T F]
      \B [T F T T]
      \C [T T F F]
      \D [T T F T]
      \E [T T T F]
      \F [T T T T])))

(defn powers-of-two []
  (map #(Math/pow 2 %) (range 100)))

(defn boolseq->num [coll]
  (prn {:bs coll})
  (->> (reverse coll)
       (map #(if %2 %1 0) (powers-of-two))
       (reduce +)
       long))

(defn hexstring->boolseq [s]
  (flatten (map hexchar->boolseq (seq s))))

(defn parse-operator-lt-1 [parse-packet version type data]
  (let [[l-data d-data] (split-at 11 data)
        p-count (boolseq->num l-data)]
    (loop [ps []
           bs d-data]
      (if (not= p-count (count ps))
        (let [{p :packet r :remaining} (parse-packet bs)]
          (recur (conj ps p) r))
        {:packet {:type type
                  :version version
                  :value nil
                  :children ps}
         :remaining bs}))))

(defn parse-operator-lt-0 [parse-packet version type data]
  (let [[l-data d-data] (split-at 15 data)
        length (boolseq->num l-data)
        [in-data remaining] (split-at length d-data)]
    (loop [ps []
           bs in-data]
      (if (seq bs)
        (let [{p :packet r :remaining} (parse-packet bs)]
          (recur (conj ps p) r))
        {:packet {:type type
                  :version version
                  :value nil
                  :children ps}
         :remaining remaining}))))


(defn parse-operator [parse-packet version type data]
  (let [length-type (first data)]
    (if length-type
      (parse-operator-lt-1 parse-packet version type (rest data))
      (parse-operator-lt-0 parse-packet version type (rest data)))))

(defn parse-constant [version type data]
  (loop [bs []
         [curr remaining] (split-at 5 data)]
    (if (first curr)
      (recur (concat bs (rest curr)) (split-at 5 remaining))
      {:packet {:type type
                :version version
                :value (boolseq->num (concat bs (rest curr)))
                :children nil}
       :remaining remaining})))

(defn parse-packet [bs]
  (let [version (boolseq->num (take 3 bs))
        type (boolseq->num (take 3 (drop 3 bs)))
        data (drop 6 bs)]
    (if (= 4 type)
      (parse-constant version type data)
      (parse-operator parse-packet version type data))))

(defn sum-versions [p]
  (if (:children p)
    (+ (:version p) (reduce + (map sum-versions (:children p))))
    (:version p)))

(defn gt [a b]
  (if (> a b) 1 0))

(defn lt [a b]
  (if (< a b) 1 0))

(defn eq [a b]
  (if (= a b) 1 0))

(defn calc-packet [p]
  (case (:type p)
    0 (reduce + (map calc-packet (:children p)))
    1 (reduce * (map calc-packet (:children p)))
    2 (reduce min (map calc-packet (:children p)))
    3 (reduce max (map calc-packet (:children p)))
    4 (:value p)
    5 (reduce gt (map calc-packet (:children p)))
    6 (reduce lt (map calc-packet (:children p)))
    7 (reduce eq (map calc-packet (:children p)))
    :else -1))

(defn task-a [fname]
  (-> fname slurp hexstring->boolseq parse-packet :packet sum-versions))
(defn task-b [fname]
  (-> fname slurp hexstring->boolseq parse-packet :packet calc-packet))

(comment
  (task-a "data/day16.input.txt")
  (task-b "data/day16.input.txt")
  
  (sum-versions (:packet (parse-packet (hexstring->boolseq "8A004A801A8002F478"))))
  (sum-versions (:packet (parse-packet (hexstring->boolseq "620080001611562C8802118E34"))))
  (sum-versions (:packet (parse-packet (hexstring->boolseq "C0015000016115A2E0802F182340"))))
  (sum-versions (:packet (parse-packet (hexstring->boolseq "A0016C880162017C3686B18A3D4780"))))
  
  (calc-packet (:packet (parse-packet (hexstring->boolseq "C200B40A82"))))
  (calc-packet (:packet (parse-packet (hexstring->boolseq "04005AC33890"))))
  (calc-packet (:packet (parse-packet (hexstring->boolseq "880086C3E88112"))))
  (calc-packet (:packet (parse-packet (hexstring->boolseq "CE00C43D881120"))))
  (calc-packet (:packet (parse-packet (hexstring->boolseq "D8005AC2A8F0"))))
  (calc-packet (:packet (parse-packet (hexstring->boolseq "F600BC2D8F"))))
  (calc-packet (:packet (parse-packet (hexstring->boolseq "9C005AC2F8F0"))))
  (calc-packet (:packet (parse-packet (hexstring->boolseq "9C0141080250320F1802104A08")))))