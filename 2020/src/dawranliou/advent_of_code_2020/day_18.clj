(ns dawranliou.advent-of-code-2020.day-18
  (:require [clojure.edn :as edn]
            [clojure.walk :as w]
            [dawranliou.aoc :as aoc]))

(defn parse [s]
  (edn/read-string (str "(" s ")")))

(defn sequential-eval [form]
  (reduce (fn [total [op n]]
            ((resolve op) total n))
          (first form)
          (partition 2 (rest form))))

(defn walk-math [eval-fn l]
  (w/postwalk #(if (sequential? %) (eval-fn %) %) l))

(walk-math sequential-eval '(1 + 2 * 3 + 4 * 5 + 6))
;; => 71
(walk-math sequential-eval '(1 + (2 * 3) + (4 * (5 + 6))))
;; => 51

;; part 1
(aoc/with-line "day-18.txt"
  (comp (partial walk-math sequential-eval) parse)
  #(apply + %))
;; => 202553439706

(defn insert-parens [form]
  (->> (partition-by (partial = '*) form)
       (replace {'(*) '*})))

(walk-math insert-parens '(1 + 2 * 3 + 4 * 5 + 6))
;; => ((1 + 2) * (3 + 4) * (5 + 6))
(->> '(1 + 2 * 3 + 4 * 5 + 6)
     (walk-math insert-parens)
     (walk-math sequential-eval))
;; => 231

;; part 2
(aoc/with-line "day-18.txt"
  (comp
    (partial walk-math sequential-eval) (partial walk-math insert-parens) parse)
  #(apply + %))
;; => 88534268715686
