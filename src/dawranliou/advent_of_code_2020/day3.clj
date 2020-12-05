(ns dawranliou.advent-of-code-2020.day3
  (:require [clojure.java.io :as io]))

(def input "inputs/2020/day3.txt")

(def unit-forest
  (with-open [rdr (io/reader input)]
    (->> (line-seq rdr)
         (map #(into [] %))
         (into []))))

(def unit-height
  (count unit-forest))

(def unit-width
  (count (first unit-forest)))

(def tree? (partial = \#))

(defn forest [[row col]]
  (->> [row (mod col unit-width)]
       (get-in unit-forest)))

(comment

  (forest [0 0])
  (forest [2 6])

  (forest [0 (+ unit-width 0)])

  )

(defn path [right down]
  (->>
    ;; start at (0, 0)
    [0 0]
    (iterate (fn [[y x]] [(+ y down) (+ x right)]))
    (take-while (fn [[y x]] (< y unit-height)))))

(comment
  ;; part 1
  (->> (path 3 1)
       (map forest)
       (filter tree?)
       count)
  ;; => 230

  )

(defn tree-count [[right down]]
  (->> (path right down)
       (map forest)
       (filter tree?)
       count))

(comment

  ;; part 2
  (->> [[1 1] [3 1] [5 1] [7 1] [1 2]]
       (map tree-count)
       (apply *))
  ;; => 9533698720
  )
