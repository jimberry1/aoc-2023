(ns aoc-2023.core
  (:require [clojure.string :as str]
            [aoc-2023.utils :as utils]))

(def test-input (utils/input-file->rows "src/aoc_2023/day_3/example.txt"))
(def day-3-input (utils/input-file->rows "resources/input_3.txt"))

(defn line->part-and-symbol-col [line-number line]
  (loop [results [] index 0]
    (let [head (nth line index nil)]
      (cond
        ;; If nil the end of the line has been reached
        (nil? head) results

        ;; if a digit, form full serial number, add to results and recur
        (Character/isDigit head)
        (let [number (re-find #"\d+" (->> line
                                          (drop index)
                                          (apply str)))
              end-of-number-index (+ index (count number))]
          (recur
           (conj results {:value (Integer/parseInt number)
                          :y line-number
                          :x (range index end-of-number-index)
                          :is-symbol? false})
           end-of-number-index))

        ;; if it's not a . it must be a symbol, so add to coll and return
        (not= head \.) (recur (conj results
                                    {:value nil
                                     :y line-number
                                     :x index
                                     :is-symbol? true})
                              (inc index))

        ;; default is a dot, just recur in this case
        :else (recur results (inc index))))))

(defn close-enough? [val-1 val-2]
  (<= (abs (- val-1 val-2)) 1))

(defn symbol->neighbour-numbers [{:keys [x y] :as symbol} all-values]
  (->> all-values
       (remove :is-symbol?)
       (filter (fn [node]
                 (and (some #(close-enough? x %) (:x node))
                      (close-enough? y (:y node)))))))

(defn input->neighbour-parts [file]
  (let [parsed-map (->> file
                        (map-indexed line->part-and-symbol-col)
                        flatten)]
    (->> parsed-map
         (filter :is-symbol?)
         (map #(symbol->neighbour-numbers % parsed-map)))))

(comment
  ;; part 1 
  (->> test-input
       input->neighbour-parts
       flatten
       set
       (reduce (fn [acc val]
                 (+ acc (:value val))) 0))

  ;; p2 
  (->> day-3-input
       input->neighbour-parts
       (remove #(< (count %) 2))
       (reduce (fn [acc val]
                 (let [gear-power (->> val
                                       (map :value)
                                       (apply *))]
                   (+ acc gear-power))) 0)))