(def line-grammar
  ~{:main (* (group (repeat 5 :card)) :s (number :d+))
    :card (choice
            (/ "A" 14)
            (/ "K" 13)
            (/ "Q" 12)
            (/ "J" 11)
            (/ "T" 10)
            (number :d))})

(def parsed
  (with [f (file/open #"input/day-07-sample.txt"
                      "input/day-07-input.txt"
                      )]
    (seq [line :in (file/lines f)]
      (peg/match line-grammar line))))

(defn rank [hand]
  (let [freqs (frequencies hand)
        group-counts (sorted (values freqs) >)]
    [# :type
     (match group-counts
       [5] 7                    # 7: 5 of a kind
       [4 1] 6                  # 6: 4 of a kind
       [3 2] 5                  # 5: full house
       [3 1 1] 4                # 4: 3 of a kind
       [2 2 1] 3                # 3: two pairs
       [2 1 1 1] 2              # 2: one pair
       [1 1 1 1 1] 1            # 1: high card
       )
     ;hand]))

(def part-1
  (sum (seq [[i [hand bet]]
             :in (pairs (sorted-by (comp rank first) parsed))]
         (* (inc i) bet))))

(def parsed-part-2
  (with [f (file/open #"input/day-07-sample.txt"
                      "input/day-07-input.txt"
                      )]
    (seq [line :in (file/lines f)]
      (peg/match ~{:main (* (group (repeat 5 :card)) :s (number :d+))
                   :card (choice
                           (/ "A" 14)
                           (/ "K" 13)
                           (/ "Q" 12)
                           (/ "J" 1)
                           (/ "T" 10)
                           (number :d))}
                 line))))

(defn rank-with-jokers [hand]
  (let [freqs (frequencies hand)
        jokers (get freqs 1 0)
        _ (put freqs 1 nil)     # remove joker counts
        group-counts (sorted (values freqs) >)
        [leading-count & rest-counts] group-counts]
    [# :type

     # # 1 joker
     # [4]
     # [3 1]
     # [2 2]
     # [2 1 1]
     # [1 1 1 1]
     # # 2 jokers
     # [3]
     # [2 1]
     # [1 1 1]
     # # 3 jokers
     # [2]
     # [1 1]
     # # 4 jokers
     # [1]
     # # 5 jokers
     (match [(+ jokers (or leading-count 0)) ;rest-counts]
       [5] 7                    # 7: 5 of a kind
       [4 1] 6                  # 6: 4 of a kind
       [3 2] 5                  # 5: full house
       [3 1 1] 4                # 4: 3 of a kind
       [2 2 1] 3                # 3: two pairs
       [2 1 1 1] 2              # 2: one pair
       [1 1 1 1 1] 1            # 1: high card
       )
     ;hand]))

(def part-2
  (sum (seq [[i [hand bet]]
             :in (->> parsed-part-2
                      (sorted-by (comp rank-with-jokers first))
                      pairs)]
         (* (inc i) bet))))
