(ns aoc-2023.day-5.core
  (:require [clojure.string :as str]
            [aoc-2023.utils :as utils]
            [clojure.set :as set]))

(def test-input (utils/input-file->rows "src/aoc_2023/day_5/example.txt"))
(def day-5-input (utils/input-file->rows "resources/input_5.txt"))

(defn input->group [input]
  (->> input
       (partition-by #(= % ""))
       (remove #(= (first %) ""))))

(defn number-row->map [number-row]
  (let [[dest-range-start source-range-start range-length] (utils/string->digit-seq number-row)]
    {:from source-range-start :to (+ source-range-start range-length) :offset (- dest-range-start source-range-start)}))

(defn fetch-dest-from-map [loc-map location-val]
  (reduce (fn [acc {:keys [from to offset]}]
            (if (and
                 (>= location-val from)
                 (<= location-val to))
              (reduced (+ location-val offset))
              acc))
          location-val
          loc-map))

(defn parse-map-name [unparsed-map-name]
  (let [relevant-bit (-> unparsed-map-name
                         (str/split #" ")
                         first)
        name-segements (-> relevant-bit
                           (str/split #"-"))]
    {:from (first name-segements)
     :to (last name-segements)}))

(defn parse-map [unparsed-map]
  (let [map-name (-> unparsed-map first parse-map-name)
        map-vals (->> unparsed-map
                      rest
                      (map number-row->map))]
    (assoc map-name :loc-map map-vals)))

(defn parse-input [input]
  (let [[seeds & unparsed-maps] (input->group input)
        seed-nums (-> seeds first utils/string->digit-seq)
        seed-maps (map parse-map unparsed-maps)]
    [seed-nums seed-maps]))

(defn cur-key->location-map [input-maps from-key]
  (reduce (fn [_acc input-map]
            (when (= from-key (:from input-map))
              (reduced input-map)))
          nil
          input-maps))

(defn seed->location [input-maps seed-number]
  (loop [cur-key "seed" location seed-number]
    (if (= cur-key "location")
      location
      (let [relevant-map (cur-key->location-map input-maps cur-key)]
        (recur (:to relevant-map) (fetch-dest-from-map (:loc-map relevant-map) location))))))

;; PART 1
(let [[seeds location-maps] (parse-input day-5-input)]
  (->> seeds
       (map #(seed->location location-maps %))
       (apply min)))

;; PART 2
(defn seed-row->seed-number-range [seed-row]
  (loop [acc-seeds [] remaining-row seed-row]
    (if (empty? remaining-row)
      acc-seeds
      (let [[range-start range-length & rest-seeds] remaining-row]
        (recur (conj acc-seeds {:from range-start :to (+ range-start range-length)}) rest-seeds)))))

(defn parse-input-p2 [input]
  (let [[seeds & unparsed-maps] (input->group input)
        seed-nums (-> seeds first utils/string->digit-seq)
        all-seed-nums (seed-row->seed-number-range seed-nums)
        seed-maps (map parse-map unparsed-maps)]
    [all-seed-nums seed-maps]))

(defn ->offset-adjusted-range
  "Applies the offset of a range to the :from and :to keys
   e.g. {:from 0 :to 5 :offset 10} -> {:from 10 :to 15}"
  [{:keys [from to offset] :as _range-with-offset}]
  {:from (+ from offset) :to (+ to offset)})

(defn range->ordered-mappings-in-range
  "Returns a sorted collection of all mappings for a given directory 
   that fully or partially overlap with the provided number range"
  [{range-from :from range-to :to :as _last-range} all-mappings]
  (->> all-mappings
       (remove (fn [{mapping-from :from mapping-to :to}]
                 (or
                  (<= mapping-to range-from) ;; completely before
                  (>= mapping-from range-to) ;; completely after
                  )))
       (sort-by :from)))

(defn chain-ranges
  [{range-from :from range-to :to} starting-range all-mappings-for-next-location]
  (loop [results #{starting-range} remaining-nodes all-mappings-for-next-location]
    (let [{cur-from :from cur-to :to cur-offset :offset :as cur-mapping} (first remaining-nodes)
          {next-from :from :or {next-from range-to}} (second remaining-nodes)]
      (if (nil? cur-mapping)
        results
        (let [node {:from (max range-from cur-from) :to (min range-to cur-to) :offset cur-offset}
              optional-extra-node (when (< cur-to (min range-to next-from)) {:from cur-to :to (min range-to next-from) :offset 0})
              new-nodes (if optional-extra-node #{node optional-extra-node} #{node})]
          (recur (set/union results new-nodes) (rest remaining-nodes)))))))

(defn range->sub-ranges
  "Returns a collection of maps representing the subdivisions of a range according to partitioned offsets,
     e.g. for range {:from 0 :to 10} and all-mappings [{:from 2 :to 5 :offset 30}]
     returns [{:from 0 :to 1} {:from 32 :to 35} {:from 6 :to 10}]"
  [all-mappings {range-from :from range-to :to :as full-range}]
  (let [all-in-range (range->ordered-mappings-in-range full-range all-mappings)
        {first-from :from first-to :to first-offset :offset :as first-node} (first all-in-range)
        first-sub-range (cond
                          (nil? first-node) {:from range-from :to range-to :offset 0}
                          (> first-from range-from) {:from range-from :to first-from :offset 0}
                          :else {:from range-from :to (min first-to range-to) :offset first-offset})]
    (chain-ranges full-range first-sub-range all-in-range)))

(defn combine-ranges [last-range all-mappings]
  (->> last-range
       (range->sub-ranges all-mappings)
       (map ->offset-adjusted-range)))

(defn seed->location-p2 [input-maps seed-range]
  (loop [cur-key "seed" location-ranges [seed-range]]
    (if (= cur-key "location")
      location-ranges
      (let [relevant-map (cur-key->location-map input-maps cur-key)]
        (recur (:to relevant-map) (mapcat #(combine-ranges % (:loc-map relevant-map)) location-ranges))))))

;; PART 2 ANSWER
(let [[seeds location-maps] (parse-input-p2 day-5-input)]
  (->> seeds
       (mapcat #(seed->location-p2 location-maps %))
       (map :from)
       (apply min)))