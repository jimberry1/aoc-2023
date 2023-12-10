(ns aoc-2023.day-9.core
  (:require [aoc-2023.utils :as utils]))

(def test-input (utils/input-file->rows "src/aoc_2023/day_9/example.txt"))
(def real-input (utils/input-file->rows "resources/input_9.txt"))

(defn measurements->measurement-deltas [digits]
  (loop [nums digits diffs '()]
    (let [[first next] nums]
      (if (nil? next)
        (reverse diffs)
        (recur (rest nums) (conj diffs (- next first)))))))

(defn measurements->recursive-deltas [seq-digits]
  (loop [seqs (list seq-digits)]
    (let [diffs (-> seqs first measurements->measurement-deltas)
          res-set (conj seqs diffs)]
      (if (or (empty? diffs) (every? #(= % 0) diffs))
        res-set
        (recur res-set)))))

(defn seqs->pred-value [seqs]
  (->> seqs (map last) (reduce +)))

(defn line->seq [line]
  (-> line (utils/string->digit-seq :allow-negatives true) measurements->recursive-deltas))

(->> real-input
     (map line->seq)
     (map seqs->pred-value)
     (reduce +))

;; Part 2
(defn seqs->pred-prev [seqs]
  (let [[first-row & rest-rows] seqs
        delta (loop [remaining-seqs (reverse rest-rows) acc 0]
                (if (empty? remaining-seqs)
                  acc
                  (recur (rest remaining-seqs) (- (ffirst remaining-seqs) acc))))]
    (- (first first-row) delta)))

(->> real-input
     (map line->seq)
     (map seqs->pred-prev)
     (reduce +))
