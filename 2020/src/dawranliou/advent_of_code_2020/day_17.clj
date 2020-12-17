(ns dawranliou.advent-of-code-2020.day-17
  (:require [clojure.string :as str]))

(def test-input ".#.
..#
###")

(defn index-3d [input]
  (->> (str/split-lines input)
       (mapcat (fn [y xs]
                 (keep-indexed (fn [x ch] (when (= ch \#) [0 y x])) xs))
               (range))
       set))

(defn neighbors-3d [[z y x]]
  (for [dz [-1 0 1]
        dy [-1 0 1]
        dx [-1 0 1]
        :when (not= [0 0 0] [dz dy dx])]
    [(+ z dz) (+ y dy) (+ x dx)]))

(defn step [neighbors state]
  (set (for [[pos n] (frequencies (mapcat neighbors state))
             :when   (or (and (state pos)
                            (#{2 3} n))
                       (and ((complement state) pos)
                            (= 3 n)))]
         pos)))

(->> test-input
     index-3d
     (iterate (partial step neighbors-3d))
     (drop 6)
     first
     count)

;; part 1
(->> (slurp "inputs/day-17.txt")
     index-3d
     (iterate (partial step neighbors-3d))
     (drop 6)
     first
     count)
;; => 207

(defn index-4d [input]
  (->> (str/split-lines input)
       (mapcat (fn [y xs]
                 (keep-indexed (fn [x ch] (when (= ch \#) [0 0 y x])) xs))
               (range))
       set))

(defn neighbors-4d [[w z y x]]
  (for [dw [-1 0 1]
        dz [-1 0 1]
        dy [-1 0 1]
        dx [-1 0 1]
        :when (not= [0 0 0 0] [dw dz dy dx])]
    [(+ w dw) (+ z dz) (+ y dy) (+ x dx)]))

(->> (slurp "inputs/day-17.txt")
     index-4d
     (iterate (partial step neighbors-4d))
     (drop 6)
     first
     count)
;; => 2308
