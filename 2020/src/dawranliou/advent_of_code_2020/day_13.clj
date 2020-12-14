(ns dawranliou.advent-of-code-2020.day-13
  (:require [dawranliou.aoc :as aoc]
            [clojure.string :as str]))

(def input
  (aoc/with-input "day-13.txt" (comp vec line-seq)))

(def earliest
  (Integer/parseInt (first input)))

(def available-buses
  (->> (second input)
       (re-seq #"\d+" )
       (map #(Integer/parseInt %))))

(defn next-bus-of [ts bus]
  (->> (range)
       (map #(* bus %))
       (drop-while #(< % ts))
       first))

;; Part 1
(let [[n earliest-bus]
      (->> available-buses
           (map #(next-bus-of earliest %))
           (map-indexed (fn [idx itm] [idx itm]))
           (apply min-key second))]
  (* (- earliest-bus earliest)
     (nth available-buses n)))
;; => 1895

(def indexed-buses
  (->> (str/split (second input) #",")
       (map-indexed (fn [idx itm] {:t idx :id itm}))
       (filter (fn [{:keys [id]}] (not= id "x")))
       (map (fn [{:keys [id] :as bus}]
              (assoc bus :id (Integer/parseInt id))))))

;; Part 2
(->> indexed-buses
     #_[{:id 7 :t 0} {:id 13 :t 1} {:id 59 :t 4} {:id 31 :t 6} {:id 19 :t 7}]
     (reduce
       (fn [{t1 :t id1 :id :as bus1}
            {t2 :t id2 :id :as bus2}]
         (let [new-t (->> (iterate #(+ id1 %) t1)
                          (filter #(= (mod (- id2 t2) id2) (mod % id2)))
                          first)]
           {:id (* id1 id2)
            :t  new-t})))
     :t)
;; => 840493039281088
