(ns aoc-2023.day-17.core
  (:require [clojure.string :as str]
            [aoc-2023.utils :as utils]))

(def test-input (utils/input-file->rows "src/aoc_2023/day_17/example.txt"))
(def real-input (utils/input-file->rows "resources/input_17.txt"))

(def my-map (utils/indexed-grid test-input :tile-fn #(Integer/parseInt (str %))))
