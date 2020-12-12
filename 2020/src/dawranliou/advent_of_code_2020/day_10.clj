(ns dawranliou.advent-of-code-2020.day-10
  (:require [dawranliou.aoc :as aoc]))

(defn jolt-differences [jolts]
  (let [all-jolts (-> jolts
                      (conj 0)
                      (conj (+ 3 (apply max jolts))))]
    (->> all-jolts
         sort
         (partition 2 1)
         (map (fn [[n m]] (- m n)))
         frequencies)))

(jolt-differences [28 33 18 42 31 14 46 20 48 47 24 23 49 45 19 38 39 11 1 32 25
                   35 8 17 7 9 4 2 34 10 3])
;; => {1 22, 3 10}

;; part 1
(->> (aoc/with-line "day-10.txt" #(Integer/parseInt %) jolt-differences)
     vals
     (reduce *))
;; => 1836

;; (1) -> 1
;; (1 2) -> 1
;; (1 2 3) -> (1 2 3), (1 3) -> 2
;; (1 2 3 4) -> (1 2 3 4), (1 2 4), (1 3 4), (1 4) -> f(4) = 4 = 2^2 - 0
;; (1 2 3 4 5) -> (1 2 3 4 5), (1 2 3 5), (1 2 4 5), (1 3 4 5), (1 2 5), (1 3 5), (1 4 5) -> f(5) = 7 = 2^3 - 1
;; (1 2 3 4 5 6) -> (1 2 3 4 5 6), (1 2 3 4 6) * 4, (1 2 3 6) * 5 -> 13 = f(6) = 2^4 - 1 - 2
;; (1 2 3 4 5 6 7) -> (1 2 3 4 5 6 7), (1 2 3 4 5 7) * 5, (1 2 3 4 7) * 4, f(7) = 2^5 - 1 - 2 - 3 = 2^5 - (1 + 3) * 3 / 2 = 32 - 6 = 26

(defn combs [x]
  (- (reduce * (repeat (- x 2) 2))
     (if (> x 4)
       (let [y (- x 4)]
         (/ (* (+ 1 y) y) 2))
       0)))

(combs 5)
(combs 4)
(combs 3)
(combs 2)

(defn adapter-combinations [jolts]
  (let [all-jolts (-> jolts
                      (conj 0))]
    (->> all-jolts
         sort
         (partition 2 1)
         (map (fn [[n m]] (- m n)))
         (partition-by (partial = 1))
         (map frequencies)
         (keep (fn [counts]
                 (when-let [c (get counts 1)]
                   (when (pos? c)
                     (inc c)))))
         (map combs)
         (reduce *))))

(adapter-combinations
  [28 33 18 42 31 14 46 20 48 47 24 23 49 45 19 38 39 11 1 32 25 35 8 17 7 9 4 2
   34 10 3])
;; => 19208

;; part 2
(time
  (aoc/with-line "day-10.txt" #(Integer/parseInt %) adapter-combinations))
;; "Elapsed time: 1.16457 msecs"
;; => 43406276662336
