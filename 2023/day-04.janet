(def card-grammar
  ~{:main (sequence "Card" :s+ (number :d+) ":" :s+
                    (group :numbers) "|" :s+
                    (group :numbers))
    :numbers (some (sequence (number :d+) (? :s+)))})

(def cards
  (with [f (file/open #"input/day-04-sample.txt"
                      "input/day-04-input.txt"
                      )]
    (seq [l :in (file/lines f)]
      (peg/match card-grammar l))))

# Part 1
(sum (seq [[_id winning-nums my-nums] :in cards
           :let [win? |(has-value? winning-nums $)
                 bingo (filter win? my-nums)
                 points (length bingo)]
           :when (pos? points)]
       (math/exp2 (dec points))))

# Part 2
(def card-counts "card id -> count"
  @{})

(loop [id :range-to [1 (length cards)]]
  (put card-counts id 1))

(loop [[id winning-nums my-nums] :in cards
       :let [current-id-copies (get card-counts id)
             win? |(has-value? winning-nums $)
             bingo (filter win? my-nums)
             n-copies (length bingo)]
       copy-id :range-to [(+ id 1) (+ id n-copies)]]
  (update card-counts copy-id + current-id-copies))

(sum (values card-counts))
