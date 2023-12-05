(ns aoc-2023.day-4.core
  (:require [clojure.string :as str]
            [aoc-2023.utils :as utils]))

(def test-input (utils/input-file->rows "src/aoc_2023/day_4/example.txt"))
(def day-4-input (utils/input-file->rows "resources/input_4.txt"))

(defn numbers->winners-and-actual [input]
  (let [index-of-line (str/index-of input "|")
        winners (re-seq #"\d+" (subs input 0 index-of-line))
        actual (re-seq #"\d+" (subs input index-of-line))]
    [winners actual]))

(defn card->score [card]
  (reduce (fn [score winner]
            (if (= score 0) 1 (* score 2)))
          0
          card))

(defn row->card [row]
  (let [[game-info number-info] (str/split row #": ")
        [winning-nums actual-nums] (numbers->winners-and-actual number-info)
        winner? (set winning-nums)]
    (->> actual-nums
         (filter winner?)
         card->score)))

;; part 1 
(->> day-4-input
     (map row->card)
     (apply +))

;; part 2
(defn card-map->duplicated-cards [initial-card-map]
  (loop [card-map initial-card-map remaining-keys (-> initial-card-map keys sort)]
    (if (empty? remaining-keys)
      card-map
      (let [card-key (first remaining-keys)
            next-card-key (inc card-key)
            {:keys [count winners-count]} (card-map card-key)
            won-card-keys (range next-card-key (+ next-card-key (clojure.core/count winners-count)))
            updated-map (reduce (fn [updated-map update-row-index]
                                  (update-in updated-map [update-row-index :count] #(+ % count)))
                                card-map
                                won-card-keys)]
        (recur updated-map (rest remaining-keys))))))

(defn row->card-2 [row]
  (let [[game-info number-info] (str/split row #": ")
        game-number-key (Integer/parseInt (re-find #"\d+" game-info))
        [winning-nums actual-nums] (numbers->winners-and-actual number-info)
        winners-count (filter #((set winning-nums) %) actual-nums)]
    {game-number-key {:count 1 :winners-count (map #(Integer/parseInt %) winners-count)}}))

(->> day-4-input
     (map row->card-2)
     (into {})
     card-map->duplicated-cards
     vals
     (map :count)
     (apply +))