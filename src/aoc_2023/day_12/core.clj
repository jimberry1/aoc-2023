(ns aoc-2023.day-12.core
  (:require [clojure.string :as str]
            [aoc-2023.utils :as utils]))

(def test-input (utils/input-file->rows "src/aoc_2023/day_12/example.txt"))
(def real-input (utils/input-file->rows "resources/input_12.txt"))
























(defn find-all-combos [seq]
  (let []))




;; map over input and find all question marks
;; replace question marks with . or #, 
;; then validate 

;; for all valid seqs, recur with rest of seq and 1 fewer # to place



(parse-line ".??..??...?##. 1,1,3")
