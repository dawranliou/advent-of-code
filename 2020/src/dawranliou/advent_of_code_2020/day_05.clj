(ns dawranliou.advent-of-code-2020.day-05
  (:require [dawranliou.aoc :as aoc]))

(defn parse [s]
  (when-let [[_ row-code col-code] (re-matches #"([F|B]{7})([R|L]{3})" s)]
    (let [row-code->int #(Integer/parseInt (->> %
                                                (map {\F 0 \B 1})
                                                (apply str))
                                           2)
          col-code->int #(Integer/parseInt (->> %
                                                (map {\L 0 \R 1})
                                                (apply str))
                                           2)
          row           (row-code->int row-code)
          col           (col-code->int col-code)
          seat          (+ col (* row 8))]
      [row col seat])))


(comment
  (re-matches #"([F|B]{7})([R|L]{3})" "BFFFBBFRRR")
  ;; => ["BFFFBBFRRR" "BFFFBBF" "RRR"]

  (parse "BFFFBBFRRR")
  ;; => [70 7 567]
  (parse "FFFBBBFRRR")
  ;; => [14 7 119]
  (parse "BBFFBBFRLL")
  ;; => [102 4 820]

  ;; part 1
  (aoc/with-line "day-05.txt" (comp last parse) #(apply max %))
  ;; => 933

  (def seats
    (aoc/with-line "day-05.txt" (comp last parse) sort))

  ;; part 2
  (let [empty-seat? (complement (set seats))
        next-seat   (take (dec (count seats)) (map inc seats))]
    (filter empty-seat? next-seat))
  ;; => (711)
  )
