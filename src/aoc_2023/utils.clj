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