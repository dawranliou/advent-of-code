(ns dawranliou.advent-of-code-2020.day-18
  (:require [dawranliou.aoc :as aoc]
            [clojure.walk :as w]
            [clojure.edn :as edn]))

(defn parse [s]
  (edn/read-string (str "(" s ")")))

(defn eval-math [l]
  (let [pre  (first l)
        tail (rest l)]
    (reduce (fn [total [op n]]
              (({'+ + '* *} op) total (if (list? n) (eval-math n) n)))
            (if (list? pre) (eval-math pre) pre)
            (partition 2 tail))))

;; part 1
(->> (aoc/with-line "day-18.txt" (comp eval-math parse) #(apply + %)))
;; => 202553439706
