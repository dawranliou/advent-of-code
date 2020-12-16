(ns dawranliou.advent-of-code-2020.day-16
  (:require [clojure.string :as str]))

(def input
  (slurp "inputs/day-16.txt"))

(defn parse-ticket [s]
  (mapv #(Integer/parseInt %) (re-seq #"\d+" s)))

(defn parse-rule [s]
  (let [[_ rule n1 n2 n3 n4]
        (re-matches #"(.+): (\d+)-(\d+) or (\d+)-(\d+)" s)

        n1 (Integer/parseInt n1)
        n2 (Integer/parseInt n2)
        n3 (Integer/parseInt n3)
        n4 (Integer/parseInt n4)]
    {:rule   rule
     :n1     n1
     :n2     n2
     :n3     n3
     :n4     n4
     :val-fn #(or (<= n1 % n2) (<= n3 % n4))}))

(def sections
  (let [[rules your-ticket nearby-tickets]
        (str/split input #"\n\n")]
    {:rules          (mapv parse-rule (str/split-lines rules))
     :your-ticket    (parse-ticket (last (str/split-lines your-ticket)))
     :nearby-tickets (mapv parse-ticket
                           (rest (str/split-lines nearby-tickets)))}))

(defn number-valid?
  [rules n]
  (->> (map :val-fn rules)
       (some (fn [valid?] (valid? n)))))

(def number-invalid? (complement number-valid?))

(defn validate [ticket]
  (every? (fn [n] (number-valid? (:rules sections) n)) ticket))

(defn invalid-numbers [rules ticket]
  (filter (partial number-invalid? rules) ticket))

;; part 1
(->> sections
     :nearby-tickets
     (mapcat (partial invalid-numbers (:rules sections)))
     (reduce +))
;; => 30869
