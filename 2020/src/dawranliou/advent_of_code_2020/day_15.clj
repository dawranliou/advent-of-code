(ns dawranliou.advent-of-code-2020.day-15
  (:require [dawranliou.aoc :as aoc]
            [clojure.string :as str]))

(def input [1 12 0 20 8 16])

(def prev-turn
  [(->> input
        drop-last
        (map-indexed (fn [idx itm] [itm (inc idx)]))
        (into {}))
   {:turn (count input) :num (last input)}])

(def test-input [0 3 6])
(def test-prev-turn [{0 1, 3 2} {:turn 3 :num 6}])

(defn turn [[prev-turn {:keys [turn num] :as _most-recent}]]
  (if-let [num-prev-turn (prev-turn num)]
    [(assoc prev-turn num turn)
     {:turn (inc turn)
      :num  (- turn num-prev-turn)}]
    [(assoc prev-turn
            num turn)
     {:turn (inc turn)
      :num  0}]))

(->> test-prev-turn
     turn
     turn
     turn
     turn
     turn
     turn
     turn)

(->> (iterate turn test-prev-turn)
     (take 2018)
     last
     second)
;; => {:turn 2020, :num 436}

;; part 1
(->> (iterate turn prev-turn)
     (take (- 2021 (count input)))
     last
     second)
;; => {:turn 2020, :num 273}

;; part 2
(time
  (->> (iterate turn prev-turn)
       (take (- 30000001 (count input)))
       last
       second))
;; "Elapsed time: 21343.547631 msecs"
;; => {:turn 30000000, :num 47205}
