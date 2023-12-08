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
