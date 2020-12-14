(ns dawranliou.advent-of-code-2020.day-14
  (:require [dawranliou.aoc :as aoc]))

(defn parse [l]
  (if-let [[_ mask] (re-matches #"mask = ([X01]{36})" l)]
    {:type :mask
     :mask mask}
    (let [[_ address value] (re-matches #"mem\[(\d+)\] = (\d+)" l)]
      {:type    :mem
       :address (Integer/parseInt address)
       :value   (Integer/parseInt value)})))

(defn mask-value [{:keys [mask value]}]
  (->> mask
       reverse
       (map-indexed (fn [address itm] [address itm]))
       (remove (comp (partial = \X) second))
       (reduce (fn [v [n type]]
                 (case type
                   \0 (bit-clear v n)
                   \1 (bit-set v n)))
               value)))

(mask-value {:mask "1XXXX0X" :value 11})  ; => 73
(mask-value {:mask "1XXXX0X" :value 101}) ; => 101
(mask-value {:mask "1XXXX0X" :value 0})   ; => 64

(def instructions
  (->> (aoc/with-line "day-14.txt" parse vec)
       (partition-by :mask)
       (partition 2)
       (mapcat (fn [[[{:keys [mask]}] ins]]
                 (map #(assoc % :mask mask) ins)))))

;; Part 1
(->> instructions
     (map #(assoc % :masked-val (mask-value %)))
     (reduce (fn [m {:keys [address masked-val] :as itm}]
               (assoc m address masked-val))
             {})
     (reduce (fn [sum mem]
               (+ sum (second mem)))
             0))
;; => 9615006043476

(defn floating-addresses [{:keys [mask address]}]
  (let [parsed-mask (->> mask
                         reverse
                         (map-indexed (fn [address itm] [address itm])))]
    ((fn inner [mask address]
       (if-let [[n m] (last mask)]
         ;;
         (case m
           \0 (inner (drop-last mask) address)
           \1 (inner (drop-last mask) (bit-set address n))
           \X (concat (inner (drop-last mask) (bit-clear address n))
                      (inner (drop-last mask) (bit-set address n))))
         [address]))
     parsed-mask address)))

(floating-addresses {:mask    "000000000000000000000000000000X1001X"
                     :address 42})
;; => (26 27 58 59)
(floating-addresses {:mask    "00000000000000000000000000000000X0XX"
                     :address 26})
;; => (16 17 18 19 24 25 26 27)

;; Part 2
(->> instructions
     (map #(assoc % :masked-address (floating-addresses %)))
     (mapcat (fn [{:keys [masked-address value]}]
               (map #(hash-map :address % :value value) masked-address)))
     (reduce (fn [m {:keys [address value] :as itm}]
               (assoc m address value))
             {})
     (reduce (fn [sum mem]
               (+ sum (second mem)))
             0))
;; 4275496544925
