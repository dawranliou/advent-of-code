(ns dawranliou.advent-of-code-2020.day-18
  (:require [clojure.edn :as edn]
            [clojure.walk :as w]
            [dawranliou.aoc :as aoc]))

(defn parse [s]
  (edn/read-string (str "(" s ")")))

(defn eval-math [l]
  (w/postwalk
    (fn [form]
      (if (sequential? form)
        (->> (partition 2 (rest form))
             (reduce
               (fn [total [op n]]
                 ((resolve op) total n))
               (first form)))
        form))
    l))

(eval-math '(1 + 2 * 3 + 4 * 5 + 6))
;; => 71
(eval-math '(1 + (2 * 3) + (4 * (5 + 6))))
;; => 51

;; part 1
(aoc/with-line "day-18.txt" (comp eval-math parse) #(apply + %))
;; => 202553439706

(defn insert-parens [l]
  (w/postwalk
    (fn [form]
      (if (sequential? form)
        (->> (partition-by (partial = '*) form)
             (replace {'(*) '*}))
        form))
    l))

(insert-parens '(1 + 2 * 3 + 4 * 5 + 6))
;; => ((1 + 2) * (3 + 4) * (5 + 6))
(->> '(1 + 2 * 3 + 4 * 5 + 6)
     insert-parens
     eval-math)
;; => 231

;; part 2
(aoc/with-line
  "day-18.txt"
  (comp eval-math insert-parens parse)
  #(apply + %))
;; => 88534268715686
