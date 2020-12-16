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

(def rules (:rules sections))

(defn number-valid?
  [rules n]
  (->> (map :val-fn rules)
       (some (fn [valid?] (valid? n)))))

(def number-invalid? (complement number-valid?))

(defn valid-ticket? [rules ticket]
  (every? (partial number-valid? rules) ticket))

(defn invalid-numbers [rules ticket]
  (filter (partial number-invalid? rules) ticket))

;; part 1
(->> sections
     :nearby-tickets
     (mapcat (partial invalid-numbers rules))
     (reduce +))
;; => 30869

(def valid-nearby-tickets
  (->> sections
       :nearby-tickets
       (filter (partial valid-ticket? rules))))

(def ticket-number-by-position
  (->> (conj valid-nearby-tickets (:your-ticket sections))
       (apply mapv vector)))

(def pos-and-possible-rule-tuples
  (->> (for [[pos ticket-numbers]
             (map-indexed #(vector %1 %2) ticket-number-by-position)
             r     rules
             :when (every? (:val-fn r) ticket-numbers)]
         {:rule (:rule r)
          :pos  pos})
       (group-by :pos)
       (reduce-kv (fn [m k v] (assoc m k (set (map :rule v)))) {})))

(def pos-and-rule-tuples
  (->> pos-and-possible-rule-tuples
       (sort-by (comp count second))
       (reduce (fn [m [pos rs]]
                 (let [used-rules (into #{} (vals m))
                       only-rule  (first (remove used-rules rs))]
                   (assoc m pos only-rule)))
               {})
       (sort-by first)))

;; Part 2
(->> pos-and-rule-tuples
     (filter #(re-find #"departure\ " (second %)))
     (map first)
     (map #(nth (:your-ticket sections) %))
     (reduce *))
;; => 4381476149273
