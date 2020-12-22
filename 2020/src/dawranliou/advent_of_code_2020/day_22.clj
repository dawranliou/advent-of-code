(ns dawranliou.advent-of-code-2020.day-22
  (:require [clojure.string :as str]))

(let [input (slurp "inputs/day-22.txt")
      [p-1 p-2] (str/split input #"\n\n")
      d-1 (map #(Integer/parseInt %) (rest (str/split-lines p-1)))
      d-2 (map #(Integer/parseInt %) (rest (str/split-lines p-2)))]
  (def d-1 d-1)
  (def d-2 d-2))

#_ #_
(def d-1 [9 2 6 3 1])
(def d-2 [5 8 4 7 10])

(defn round [[d1 d2]]
  (let [c1 (first d1)
        c2 (first d2)]
    (if (< c1 c2)
      [(rest d1) (concat (rest d2) [c2 c1])]
      [(concat (rest d1) [c1 c2]) (rest d2)])))

;; part 1
(->> (iterate round [d-1 d-2])
     (drop-while (fn [[d1 d2]] (and (seq d1) (seq d2))))
     first
     (filter seq)
     first
     reverse
     (map-indexed #(vector (inc %1) %2))
     (map (partial reduce *))
     (reduce +))
;; => 33473
