(ns dawranliou.advent-of-code-2020.day-12
  (:require [dawranliou.aoc :as aoc]
            [clojure.string :as str]))

(defn parse [s]
  (let [[_ action value] (re-matches #"([NSEWLRF])(\d+)" s)]
    {:act (keyword action)
     :val  (Integer/parseInt value)}))

(def input
  (aoc/with-line "day-12.txt" parse vec))

(def test-input
  (->> (str/split-lines "F10
N3
F7
R90
F11")
       (map parse)
       vec))

(defn forward [[x y] [xdir ydir] value]
  [(+ x (* value xdir))
   (+ y (* value ydir))])

(forward [1 0] [0 1] 3)

{[0 1] [1 0]
 [1 0] [0 -1]
 [0 -1] [-1 0]
 [-1 0] [0 1]}

(defn turn [{:keys [dir] :as state} degree]
  (assoc state :dir (->> (iterate {[0 1]  [1 0]
                                   [1 0]  [0 -1]
                                   [0 -1] [-1 0]
                                   [-1 0] [0 1]} dir)
                         (take (inc (mod (/ degree 90) 4)))
                   last)))

(turn {:dir [0 1]} -90)

(defn step
  [{:keys [pos dir] :as state}
   {:keys [act val] :as _instruction}]
  (case act
    :F (assoc state :pos (forward pos dir val))
    :N (assoc state :pos (forward pos [0 1] val))
    :S (assoc state :pos (forward pos [0 -1] val))
    :E (assoc state :pos (forward pos [1 0] val))
    :W (assoc state :pos (forward pos [-1 0] val))
    :R (turn state val)
    :L (turn state (- val))))

(->> test-input
     (reductions step {:pos [0 0] :dir [1 0]})
     last
     :pos
     (map #(if (pos? %) % (- %)))
     (apply +))
;; => 25

;; Part 1
(->> input
     (reductions step {:pos [0 0] :dir [1 0]})
     last
     :pos
     (map #(if (pos? %) % (- %)))
     (apply +))
;; => 820
