(ns aoc-2023.day-13.core
  (:require [aoc-2023.utils :as utils]))

(def real-input (utils/input-file->grouped-rows "resources/input_13.txt" ""))

(defn rows->columns [rows]
  (loop [all-rows rows columns []]
    (if (empty? (first all-rows))
      columns
      (recur (map rest all-rows) (conj columns (apply str (map first all-rows)))))))

(defn compare-with-smudge [str-1 str-2]
  (let [num-of-entries (count str-1)
        equal-count (->> (map #(= %1 %2) str-1 str-2)
                         (filter true?)
                         count)]
    {:equal? (= equal-count num-of-entries)
     :equal-with-smudge? (>= (inc equal-count) num-of-entries)}))

(defn is-smudged-mirror?
  "Checks to see whether two groups are exactly 1 smudge away from being enantiomers"
  [group-1 group-2]
  (let [shortest-coll-length (min (count group-1) (count group-2))
        normalised-group-1 (->> group-1 reverse (take shortest-coll-length)) ;; reversed for looping simplicity
        normalised-group-2 (take shortest-coll-length group-2)]
    (loop [[first-1 & rest-1] normalised-group-1 [first-2 & rest-2] normalised-group-2 already-used-smudge? false]
      (let [{:keys [equal? equal-with-smudge?]} (compare-with-smudge first-1 first-2)]
        (cond
          (nil? first-1) already-used-smudge? ;; when seq exhausted return whether you used the smudge
          (not equal-with-smudge?) false
          equal? (recur rest-1 rest-2 already-used-smudge?)
          (and equal-with-smudge? (not already-used-smudge?)) (recur rest-1 rest-2 true))))))

(defn is-mirror?
  "Fn checks whether two groups are mirror images of each other.
   A group represents a collection of rows, e.g. [\"##..#\" \"#.#.#\"]"
  [group-1 group-2]
  (let [shortest-coll-length (min (count group-1) (count group-2))
        normalised-group-1 (->> group-1 reverse (take shortest-coll-length)) ;; reversed for looping simplicity
        normalised-group-2 (take shortest-coll-length group-2)]
    (every? identity (map #(= %1 %2) normalised-group-1 normalised-group-2))))

(defn find-mirror [rows allow-smudges?]
  (loop [remaining-splits (range 1 (count rows))]
    (let [[split-index & rest-splits] remaining-splits
          [section-1 section-2] (split-at split-index rows)]
      (cond
        ((if allow-smudges? is-smudged-mirror? is-mirror?) section-1 section-2) split-index
        (empty? rest-splits) 0
        :else (recur rest-splits)))))

(defn summarise-grid [rows allow-smudges?]
  (let [columns (rows->columns rows)
        row-mirror-index (find-mirror rows allow-smudges?)
        column-mirror-index (find-mirror columns allow-smudges?)]
    (+ (* row-mirror-index 100) column-mirror-index)))

;; part 1
(->> real-input (map #(summarise-grid % false)) (reduce +))

;; part 2
(->> real-input (map #(summarise-grid % true)) (reduce +))