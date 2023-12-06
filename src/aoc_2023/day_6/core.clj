(ns aoc-2023.day-6.core
  (:require [aoc-2023.utils :as utils]))

(def test-input (utils/input-file->rows "src/aoc_2023/day_6/example.txt"))
(def day-6-input (utils/input-file->rows "resources/input_6.txt"))

(defn parse-puzzle-input-p1 [input]
  (let [race-lengths (utils/string->digit-seq (first input))
        record-distances (utils/string->digit-seq (second input))]
    (map (fn [race-length record-distance]
           {:duration race-length :record record-distance}) race-lengths record-distances)))

(defn parse-puzzle-input-p2 [input]
  (let [race-length (->> input first utils/string->digit-seq (apply str) Long/parseLong)
        record-distance (->> input second utils/string->digit-seq (apply str) Long/parseLong)]
    {:duration race-length :record record-distance}))

(defn race->possible-outcomes [{:keys [duration] :as race}]
  (let [hold-durations (range 1 duration)]
    (map (fn [hold-duration]
           (let [remaining-time (- duration hold-duration)
                 distance-travelled (* remaining-time hold-duration)]
             (assoc race :distance distance-travelled)))
         hold-durations)))

;; Part 1
(->> day-6-input
     parse-puzzle-input-p1
     (map race->possible-outcomes)
     (map #(filter (fn [{:keys [record distance]}]
                     (> distance record)) %))
     (map count)
     (reduce *))

;; Part 2 
(->> day-6-input
     parse-puzzle-input-p2
     race->possible-outcomes
     (filter (fn [{:keys [record distance]}]
               (> distance record)))
     count)