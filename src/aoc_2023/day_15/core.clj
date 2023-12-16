(ns aoc-2023.day-15.core
  (:require [clojure.string :as str]
            [aoc-2023.utils :as utils]))

(def real-input (utils/input-file->rows "resources/input_15.txt"))

(defn custom-hash [total char]
  (let [inc-hash (+ total (int char))]
    (rem (* 17 inc-hash) 256)))

;; part 1 
(let [inputs (-> real-input first (str/split #","))
      hashed-inputs (map #(reduce custom-hash 0 %) inputs)]
  (reduce + hashed-inputs))

;; part 2
(defn ->lens-record [label focal-length] {:label label :fl focal-length})

(defn box-contains-lens? [box-contents {new-lens-label :label}]
  (reduce (fn [_acc {cur-lens-label :label}]
            (when (= new-lens-label cur-lens-label)
              (reduced true))) nil box-contents))

(defn add-lens [lens-map box-id lens]
  (let [box-contents (lens-map box-id)]
    (if (box-contains-lens? box-contents lens)
      (update lens-map box-id #(mapv (fn [{:keys [label] :as lens-record}]
                                       (if (= label (:label lens))
                                         (assoc lens-record :fl (:fl lens))
                                         lens-record)) %))
      (update lens-map box-id #(vec (conj % lens))))))

(defn remove-lens [lens-map box-id lens]
  (update lens-map box-id (fn [box-contents] (filterv #(not= (:label %) (:label lens)) box-contents))))

(defn perform-box-operation [lens-map input]
  (let [[lens-label lens-fl] (-> input (str/split  #"=|-"))
        lens-record (->lens-record lens-label lens-fl)
        box-identifier (reduce custom-hash 0 lens-label)]
    (if (str/includes? input "=")
      (add-lens lens-map box-identifier lens-record)
      (remove-lens lens-map box-identifier lens-record))))

(defn calc-focusing-power [[box-num box-contents]]
  (reduce + (map-indexed (fn [index test] (* (inc box-num) (inc index) (Integer/parseInt (:fl test)))) box-contents)))

(let [inputs (-> real-input first (str/split #","))
      lens-map (reduce (fn [lens-map input] (perform-box-operation lens-map input)) {} inputs)]
  (->> lens-map (map calc-focusing-power) (reduce +)))