(ns aoc-2023.day-8.core
  (:require [aoc-2023.utils :as utils]
            [clojure.math.numeric-tower :as extra-math]))

(def test-input (utils/input-file->rows "src/aoc_2023/day_8/example.txt"))
(def real-input (utils/input-file->rows "resources/input_8.txt"))

(defn line->node [line]
  (let [[node left right] (->> line
                               (re-seq #"[A-Z]+"))]
    {node {:L left :R right}}))

(defn parse-input [input]
  (let [directions (->> input first (map (comp keyword str)) cycle)
        nodes (->> input (drop 2) (map line->node) (into {}))]
    {:directions directions :node-map nodes}))

(defn navigate-map [node-map directions cur-node step-count]
  (let [next-node (get-in node-map [cur-node (first directions)])]
    (case (last next-node)
      \Z step-count
      (recur node-map (rest directions) next-node (inc step-count)))))

;; Part 1 
(let [{:keys [directions node-map]} (parse-input real-input)]
  (navigate-map node-map directions "AAA" 1))

;; Part 2
(let [{:keys [directions node-map]} (parse-input real-input)
      starting-nodes (->> node-map (map first) (filter #(= \A (last %))))]
  (->> starting-nodes
       (map #(navigate-map node-map directions % 1))
       (reduce extra-math/lcm)))