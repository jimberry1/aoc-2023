(ns aoc-2023.day-1.core
  (:require [aoc-2023.utils :as utils]
            [clojure.string :as str]))

;; PART 1
(def example-input
  (utils/input-file->rows "src/aoc_2023/day_1/example.txt"))

(def problem-input
  (utils/input-file->rows "resources/input_1.txt"))

(defn find-first-digit [input]
  (reduce (fn [_acc char]
            (when (Character/isDigit char)
              (reduced char)))
          nil
          input))

(defn string->pass [input]
  (let [first-digit (find-first-digit input)
        last-digit (find-first-digit (reverse input))]
    (str first-digit last-digit)))

(defn solve-problem [input]
  (->> input
       (map string->pass)
       (map #(Integer/parseInt %))
       (apply +)))

(solve-problem problem-input)


;; PART 2
;; This extends part 1 by having to check the string for text matches
;; Easy way to do this is map between strings and ints, checking each iteration through the input for presence of any of the numbers
;; not the most efficient, but doesn't add too much overhead to the solution efficiency
(def normal-number-literals {"one" 1 "two" 2 "three" 3 "four" 4 "five" 5
                             "six" 6 "seven" 7 "eight" 8 "nine" 9})

(defn is-number-literal-match? [input number-literals]
  (reduce (fn [_acc [number-literal-key number-literal-value]]
            (when (str/starts-with? input number-literal-key)
              (reduced number-literal-value)))
          nil
          number-literals))

(defn find-first-digit-part-2 [input & {:keys [number-literals] :or {number-literals normal-number-literals}}]
  (reduce (fn [input-head char]
            (let [number-literal (is-number-literal-match? input-head number-literals)]
              (cond
                (Character/isDigit char) (reduced char)
                number-literal (reduced number-literal)
                :else (subs input-head 1))))
          input
          input))

(defn string->pass-part-2 [input]
  (let [first-digit (find-first-digit-part-2 input)
        reversed-number-literals (update-keys normal-number-literals (fn [key]
                                                                       (apply str (reverse key))))
        last-digit (find-first-digit-part-2 (apply str (reverse input)) :number-literals reversed-number-literals)]
    (str first-digit last-digit)))

(->> problem-input
     (map string->pass-part-2)
     (map #(Integer/parseInt %))
     (apply +))

;; another, simpler solution
(def numbers {"1" 1 "2" 2 "3" 3 "4" 4 "5" 5 "6" 6 "7" 7 "8" 8 "9" 9})
(def reversed-number-literals (update-keys normal-number-literals (fn [key]
                                                                    (apply str (reverse key)))))

(defn find-first-digit-p2 [input term-maps]
  (second (reduce (fn [[lowest-index value] key]
                    (let [index-of-key (str/index-of input key)]
                      (if (and index-of-key (< index-of-key lowest-index))
                        [index-of-key (term-maps key)]
                        [lowest-index value])))
                  [99999 nil] ;; example index that will be overwritten instantly
                  (map first term-maps))))

(defn calibration-row->value [input]
  (str (find-first-digit-p2 input (merge numbers normal-number-literals)) (find-first-digit-p2 (apply str (reverse input)) (merge numbers reversed-number-literals))))

(->> problem-input
     (map calibration-row->value)
     (map #(Integer/parseInt %))
     (apply +))