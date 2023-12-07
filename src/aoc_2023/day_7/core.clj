(ns aoc-2023.day-7.core
  (:require [clojure.string :as str]
            [aoc-2023.utils :as utils]))

(def test-input (utils/input-file->rows "src/aoc_2023/day_7/example.txt"))
(def real-input (utils/input-file->rows "resources/input_7.txt"))

(def card-rankings {\2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9 \T 10 \J 11 \Q 12 \K 13 \A 14})

(defn order-type-comparator [[h1-first & h1-rest] [h2-first & h2-rest]]
  (let [comparison-result (compare h1-first h2-first)]
    (cond
      (or (nil? h1-first) (nil? h2-first)) comparison-result
      (= 0 comparison-result) (recur h1-rest h2-rest)
      :else comparison-result)))

(defn tie-break-comparator [card-rankings [h1-first & h1-rest] [h2-first & h2-rest]]
  (let [comparison-result (compare (card-rankings h1-first) (card-rankings h2-first))]
    (cond
      (or (nil? h1-first) (nil? h2-first)) comparison-result
      (= 0 comparison-result) (recur card-rankings h1-rest h2-rest)
      :else comparison-result)))

(defn order-hands [card-rankings {type-1 :type cards-1 :cards} {type-2 :type cards-2 :cards}]
  (let [card-comparison (order-type-comparator type-1 type-2)]
    (if (= card-comparison 0)
      (tie-break-comparator card-rankings cards-1 cards-2)
      card-comparison)))

(defn cards->hand-type [cards]
  (->> cards (group-by identity) vals (map count) sort reverse))

(defn parse-hand [determine-type hand]
  (let [[cards bid] (str/split hand #" ")]
    {:type (determine-type cards)
     :cards cards
     :bid (Integer/parseInt bid)}))

;; PART 1
(->> test-input
     (map (partial parse-hand cards->hand-type))
     (sort (partial order-hands card-rankings))
     (map-indexed (fn [index {:keys [bid]}]
                    (* (inc index) bid)))
     (reduce +))

;; PART 2 
(def card-rankings-p2 (assoc card-rankings \J 1))

(defn cards->hand-type-with-wildcards [cards]
  (let [hand-identity (group-by identity cards)
        joker-count (->> \J hand-identity vals count)
        rest-of-hand-vals (->> \J (dissoc hand-identity) vals (map count) sort reverse)
        [highest & rest-of-hand] (reverse (sort rest-of-hand-vals))]
    (if (= 5 joker-count) ;; if 5 jokers then return those, otherwise jokers mimic highest count
      '(5)
      (conj rest-of-hand (+ highest joker-count)))))

(->> real-input
     (map (partial parse-hand cards->hand-type-with-wildcards))
     (sort (partial order-hands card-rankings-p2))
     (map-indexed (fn [index {:keys [bid]}]
                    (* (inc index) bid)))
     (reduce +))