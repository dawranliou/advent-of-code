(ns dawranliou.advent-of-code-2020.day-13
  (:require [dawranliou.aoc :as aoc]))

(def input
  (aoc/with-input "day-13.txt" (comp vec line-seq)))

(def earliest
  (Integer/parseInt (first input)))

(def available-buses
  (->> (second input)
       (re-seq #"\d+" )
       (map #(Integer/parseInt %))))

(defn next-bus-of [ts bus]
  (->> (range)
       (map #(* bus %))
       (drop-while #(< % ts))
       first))

;; Part 1
(let [[n earliest-bus]
      (->> available-buses
           (map #(next-bus-of earliest %))
           (map-indexed (fn [idx itm] [idx itm]))
           (apply min-key second))]
  (* (- earliest-bus earliest)
     (nth available-buses n)))
;; => 1895
