(ns aoc-2023.utils
  (:require [clojure.string :as str]))

(defn input-file->rows [filename]
  (-> filename
      slurp
      str/split-lines))

(defn string->digit-seq [input & {:keys [allow-negatives]}]
  (->> input
       (re-seq (if allow-negatives #"-?\d+" #"\d+"))
       (map #(Long/parseLong %))))

(defn input-file->grouped-rows [filename partition-indicator]
  (->> filename
       input-file->rows
       (partition-by #(= % partition-indicator))
       (remove #(= (first %) partition-indicator))))

(defn indexed-grid [input & {:keys [tile-fn]}]
  (flatten (map-indexed (fn [y-index row]
                          (map-indexed (fn [x-index tile]
                                         {:coords {:x x-index :y y-index} :val (if tile-fn (tile-fn tile) tile)}) row)) input)))