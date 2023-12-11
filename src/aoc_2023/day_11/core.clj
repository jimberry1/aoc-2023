(ns aoc-2023.day-11.core
  (:require [aoc-2023.utils :as utils]))

(def real-input (utils/input-file->rows "resources/input_11.txt"))

(defn index-space [row-num row] (map-indexed (fn [x-index char] {:coords {:x x-index :y row-num} :val char :is-star? (= char \#)}) row))

(defn find-distance [{s-1-coords :coords} {s-2-coords :coords} x-expansion-indices y-expansion-indices expansion-coefficient]
  (let [x-distance (->> (range (min (:x s-1-coords) (:x s-2-coords)) (max (:x s-1-coords) (:x s-2-coords)))
                        (map (fn [x-index] (if (x-expansion-indices x-index) expansion-coefficient 1)))
                        (reduce +))
        y-distances (->> (range (min (:y s-1-coords) (:y s-2-coords)) (max (:y s-1-coords) (:y s-2-coords)))
                         (map (fn [y-index] (if (y-expansion-indices y-index) expansion-coefficient 1))))]
    (reduce + x-distance y-distances)))

(defn find-distances [stars x-expansion-indices y-expansion-indices expansion-coefficient]
  (loop [remaining-stars stars acc-distance 0]
    (if (empty? remaining-stars)
      acc-distance
      (let [[first-star & rest-stars] remaining-stars
            distances (reduce (fn [acc star] (+ acc (find-distance first-star star x-expansion-indices y-expansion-indices expansion-coefficient)))
                              acc-distance
                              rest-stars)]
        (recur rest-stars distances)))))

(defn indexed-space->expansion-indexes [indexed-space coord]
  (->> indexed-space
       (group-by #(-> % :coords coord))
       (reduce (fn [acc [k v]] (if (->> v (map :val) (every? #(= % \.))) (conj acc k) acc))
               #{})))

(defn solve [input expansion-coefficient]
  (let [indexed-space (->> input (map-indexed #(index-space %1 %2)) flatten)
        x-expansion (indexed-space->expansion-indexes indexed-space :x)
        y-expansion (indexed-space->expansion-indexes indexed-space :y)]
    (find-distances (filter :is-star? indexed-space) x-expansion y-expansion expansion-coefficient)))

(solve real-input 2) ;; part 1
(solve real-input 1000000) ;; part 2