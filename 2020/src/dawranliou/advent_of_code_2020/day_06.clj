(ns dawranliou.advent-of-code-2020.day-06
  (:require [dawranliou.aoc :as aoc]
            [clojure.string :as s]))

(def input (aoc/with-input "day-06.txt" slurp))

;; part 1
(->>
  (s/split input #"\n\n")
  (map #(s/replace % #"\n" ""))
  (map (comp count set))
  (reduce +))
;; => 6549

;; part 2
(->>
  (for [group (s/split input #"\n\n")]
    (let [group-count (count (s/split group #"\n"))
          votes       (-> group
                          (s/replace #"\n" "")
                          frequencies)]
      (->> votes
           (filter #(= group-count (second %)))
           count)))
  (reduce +))
;; => 3466
