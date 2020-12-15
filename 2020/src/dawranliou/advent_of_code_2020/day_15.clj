(ns dawranliou.advent-of-code-2020.day-15
  (:require [dawranliou.aoc :as aoc]
            [clojure.string :as str]))

(def input [1 12 0 20 8 16])

(def indexed-input
  (->> input
       (map-indexed (fn [idx itm] {:turn (inc idx) :num itm}))
       reverse))

(def test-input [0 3 6])
(def indexed-test [{:turn 3 :num 6}
                   {:turn 2 :num 3}
                   {:turn 1 :num 0}])

(defn turn [prevs]
  (let [{:keys [turn num] :as most-recent} (first prevs)]
    (if-let [prev-turn (:prev-turn most-recent)]
      (into [{:turn      (inc turn)
              :num       (- turn prev-turn)
              :prev-turn (->> prevs
                              (filter #(= (- turn prev-turn) (:num %)))
                              first
                              :turn)}]
            prevs)
      (into [{:turn      (inc turn)
              :num       0
              :prev-turn (->> prevs
                              (filter #(zero? (:num %)))
                              first
                              :turn)}]
            prevs))))

(->> indexed-test
     turn
     turn
     turn
     turn
     turn
     turn
     turn)

(->> (iterate turn indexed-test)
     (take 2018)
     last
     first)
;; => {:turn 2020, :num 436, :prev-turn nil}

;; part 1
(->> (iterate turn indexed-input)
     (take (- 2021 (count indexed-input)))
     last
     first)
;; => {:turn 2020, :num 273, :prev-turn nil}
