(def line-grammar
  ~{:main (* (group (repeat 5 :card)) :s (number :d+))
    :card (<- (set "AKQJT98765432"))})

(def parsed
  (with [f (file/open #"input/day-07-sample.txt"
                      "input/day-07-input.txt")]
    (seq [line :in (file/lines f)]
      (peg/match line-grammar line))))

(defn rank [hand]
  (let [freqs (frequencies hand)
        group-counts (sorted (values freqs) >)
        card-strength {"A" 14 "K" 13 "Q" 12 "J" 11 "T" 10
                       "9" 9 "8" 8 "7" 7 "6" 6 "5" 5 "4" 4
                       "3" 3 "2" 2 "1" 1}]
    [(match group-counts
       [5] 7
       [4 1] 6
       [3 2] 5
       [3 1 1] 4
       [2 2 1] 3
       [2 1 1 1] 2
       [1 1 1 1 1] 1)
     ;(map card-strength hand)]))

(def part-1
  (sum (seq [[i [hand bet]]
             :in (pairs (sorted-by (comp rank first) parsed))]
         (* (inc i) bet))))

(defn rank-with-jokers [hand]
  (let [freqs (frequencies hand)
        jokers (get freqs "J" 0)
        _ (put freqs "J" nil)     # remove joker counts
        group-counts (sorted (values freqs) >)
        [leading-count & rest-counts] group-counts
        card-strength {"A" 14 "K" 13 "Q" 12 "J" 0 "T" 10
                       "9" 9 "8" 8 "7" 7 "6" 6 "5" 5 "4" 4
                       "3" 3 "2" 2 "1" 1}]
    [(match [(+ jokers (or leading-count 0)) ;rest-counts]
       [5] 7
       [4 1] 6
       [3 2] 5
       [3 1 1] 4
       [2 2 1] 3
       [2 1 1 1] 2
       [1 1 1 1 1] 1)
     ;(map card-strength hand)]))

(def part-2
  (sum (seq [[i [hand bet]]
             :in (->> parsed
                      (sorted-by (comp rank-with-jokers first))
                      pairs)]
         (* (inc i) bet))))
