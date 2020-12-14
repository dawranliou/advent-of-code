(ns dawranliou.advent-of-code-2020.day-14
  (:require [dawranliou.aoc :as aoc]))

(defn parse [l]
  (if-let [[_ mask] (re-matches #"mask = ([X01]{36})" l)]
    {:type :mask
     :mask (->> mask
                reverse
                (map-indexed (fn [idx itm] [idx itm])))}
    (let [[_ idx val] (re-matches #"mem\[(\d+)\] = (\d+)" l)]
      {:type :mem
       :idx  (Integer/parseInt idx)
       :val  (Integer/parseInt val)})))

(defn masking [{:keys [mask val]}]
  (->> mask
       (remove (comp (partial = \X) second))
       (reduce (fn [v [n type]]
                 (case type
                   \0 (bit-clear v n)
                   \1 (bit-set v n)))
               val)))

(masking {:mask [[1 \0] [6 \1]] :val 11})  ; => 73
(masking {:mask [[1 \0] [6 \1]] :val 101}) ; => 101
(masking {:mask [[1 \0] [6 \1]] :val 0})   ; => 64

(def instructions
  (->> (aoc/with-line "day-14.txt" parse vec)
       (partition-by :mask)
       (partition 2)
       (mapcat (fn [[[{:keys [mask]}] ins]]
                 (map #(assoc % :mask mask) ins)))))

(def masked-instructions
  (->> instructions
       (map #(assoc % :masked-val (masking %)))))

(->> masked-instructions
     (reduce (fn [m {:keys [idx masked-val] :as itm}]
               (assoc m idx masked-val))
             {})
     (reduce (fn [sum mem]
               (+ sum (second mem)))
             0))
;; => 9615006043476
