(ns dawranliou.advent-of-code-2020.day-11
  (:require [dawranliou.aoc :as aoc]
            [clojure.string :as s]
            [clojure.test :as t]))

(def test-input
  (s/split-lines "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL"))

(def seat-occupied? #{\#})
(def seat-empty? #{\L})
(def empty-space? #{nil \. \L})

(defn next-seat-state [seats row col]
  (let [this           (get-in seats [row col])
        adjacents      [[(dec row)      col]
                        [(dec row) (inc col)]
                        [     row  (inc col)]
                        [(inc row) (inc col)]
                        [(inc row)      col]
                        [(inc row) (dec col)]
                        [     row  (dec col)]
                        [(dec row) (dec col)]]
        adjacent-seats (->> adjacents
                            (map (partial get-in seats))
                            (keep #{\L \#}))]
    (cond
      ;; Rule #1
      (and
        (seat-empty? this)
        (every? empty-space? adjacent-seats))
      \#
      ;; Rule #2
      (and
        (seat-occupied? this)
        (->> adjacent-seats (keep seat-occupied?) count (<= 4)))
      \L
      ;; Rule #3
      :else
      this)))

(next-seat-state test-input 0 0)

(defn next-seats-state [seats]
  (vec
    (for [row (range (count seats))]
      (apply str
             (for [col (range (count (nth seats row)))]
               (next-seat-state seats row col))))))

(defn stable-seats-state [seats]
  (->> seats
     (iterate next-seats-state)
     (partition 2 1)
     (take-while (fn [[cur-state nxt-state]]
                   (not= cur-state nxt-state)))
     (map last)
     last))

(defn count-occupied-seats [seats]
  (->> seats
       (apply str)
       (keep seat-occupied?)
       count))

(->> test-input
     stable-seats-state
     count-occupied-seats)
;; 37

;; part 1
(->> (aoc/with-line "day-11.txt" str vec)
     stable-seats-state
     count-occupied-seats)
;; => 2354
