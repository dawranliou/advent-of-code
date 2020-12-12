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
(defn empty-space? [s]
  (contains? #{nil \. \L} s))
(def seat? #{\# \L})

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

(def directions (for [r [-1 0 1]
                      c [-1 0 1]
                      :when (not= [r c] [0 0])]
                  [r c]))

(defn first-seat-in-direction [seats row col max-row max-col [row-dir col-dir]]
  (->> (iterate (fn [[r c]] [(+ r row-dir) (+ c col-dir)]) [row col])
       rest
       (take-while (fn [[r c]] (and (< -1 r max-row) (< -1 c max-col))))
       (map #(get-in seats %))
       (filter #(seat? %))
       first))

(first-seat-in-direction test-input 0 0 10 10 [1 1])
(first-seat-in-direction test-input 0 0 10 10 [0 -1])

(defn first-seats-in-all-directions [seats row col max-row max-col]
  (map #(first-seat-in-direction seats row col max-row max-col %) directions))

(every? empty-space? (first-seats-in-all-directions test-input 0 0 10 10))
(get-in test-input [0 0])

;; part 2
;; Override the var for convenience
(defn next-seat-state [seats row col]
  (let [this           (get-in seats [row col])
        max-row        (count seats)
        max-col        (count (first seats))
        seats-in-sight (first-seats-in-all-directions
                         seats row col max-row max-col)]
    (cond
      ;; Rule #1
      (and
        (seat-empty? this)
        (every? empty-space? seats-in-sight))
      \#
      ;; Rule #2
      (and
        (seat-occupied? this)
        (->> seats-in-sight (keep seat-occupied?) count (<= 5)))
      \L
      ;; Rule #3
      :else
      this)))

(->> (aoc/with-line "day-11.txt" str vec)
     stable-seats-state
     count-occupied-seats)
;; => 2072
