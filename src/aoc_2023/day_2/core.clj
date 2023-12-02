(ns day_2.core
  (:require [aoc-2023.utils :as utils]
            [clojure.string :as str]))

(def input (utils/input-file->rows "resources/input_2.txt"))
(def example-input (utils/input-file->rows "src/aoc_2023/day_2/example.txt"))
(def max-dice-validation {"red" 12 "green" 13 "blue" 14})

(defn- colour-section->score-map [colour-section]
  (let [trimmed-string (str/trim colour-section)
        [amount colour] (str/split trimmed-string #" ")]
    {colour (Integer/parseInt amount)}))

(defn- game-segment->colour-count
  "Transforms a game round, e.g. \"20 red, 10 blue\" to a 
  map of keys (colour) to amount"
  [game-segment]
  (let [colour-sections (str/split game-segment #",")]
    (->> colour-sections
         (map colour-section->score-map)
         (into {}))))

(defn- game-info->number [game-info]
  (-> game-info
      (str/split #" ")
      second
      Integer/parseInt))

(defn find-max-per-colour [col-key game-hands]
  (->> game-hands
       (map #(% col-key))
       (remove nil?)
       (apply max)))

(defn validate-game [game-score]
  (reduce (fn [valid? [col max-available-dice]]
            (let [max-dice-used-in-hand (find-max-per-colour col game-score)]
              (and valid? (<= max-dice-used-in-hand max-available-dice))))
          true
          max-dice-validation))

(defn game-line->score [game-line]
  (let [[game-info score-info] (str/split game-line #":")
        hands-for-game (str/split score-info #";")
        combined-score-map (map game-segment->colour-count hands-for-game)]
    [(game-info->number game-info) (validate-game combined-score-map)]))

(->> input
     (map game-line->score)
     (filter second)
     (map first)
     (apply +))

;; Part 2
(defn find-set-power
  "Very similar to validation from part 1, but instead returns the power, 
   not a boolean indication of whether the combination is possible."
  [game-score]
  (reduce (fn [acc [col max-available-dice]]
            (let [max-dice-used-in-hand (find-max-per-colour col game-score)]
              (* acc max-dice-used-in-hand)))
          1 ;; start with multiplier of 1 to not influence score on first iteration
          max-dice-validation))

(defn game-line->set-power [game-line]
  (let [score-segment (second (str/split game-line #":"))
        game-rounds (str/split score-segment #";")
        score-map (map game-segment->colour-count game-rounds)]
    (find-set-power score-map)))

(->> input
     (map game-line->set-power)
     (apply +))