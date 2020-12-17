(ns dawranliou.advent-of-code-2020.day-17
  (:require [clojure.string :as str]))

(def test-input ".#.
..#
###")

(defn index [input]
  (->> (str/split-lines input)
       (mapcat (fn [y xs]
                 (keep-indexed (fn [x ch] (when (= ch \#) [0 y x])) xs))
               (range))
       set))

(defn neighbors [[z y x]]
  (for [dz [-1 0 1]
        dy [-1 0 1]
        dx [-1 0 1]
        :when (not= [0 0 0] [dz dy dx])]
    [(+ z dz) (+ y dy) (+ x dx)]))

(defn step [state]
  (set (for [[pos n] (frequencies (mapcat neighbors state))
             :when   (or (and (state pos)
                            (#{2 3} n))
                       (and ((complement state) pos)
                            (= 3 n)))]
         pos)))

(->> test-input
     index
     (iterate step)
     (drop 6)
     first
     count)

;; part 1
(->> (slurp "inputs/day-17.txt")
     index
     (iterate step)
     (drop 6)
     first
     count)
;; => 207
