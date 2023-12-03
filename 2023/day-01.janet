# Part 1

(def grammar-part-1
  ~{:main (any (choice (number :d) :a))})

(with [f (file/open #"input/day-01-sample.txt"
                    "input/day-01-input.txt"
                    )]
  (->> (seq [line :iterate (file/read f :line)]
         (peg/match grammar-part-1 line))
       (map |(+ (* 10 (first $)) (last $)))
       (reduce2 +)))

# Part 2

(def grammar-part-2
  ~{:digit (choice
             (/ (if "one" 1) 1)
             (/ (if "two" 1) 2)
             (/ (if "three" 1) 3)
             (/ (if "four" 1) 4)
             (/ (if "five" 1) 5)
             (/ (if "six" 1) 6)
             (/ (if "seven" 1) 7)
             (/ (if "eight" 1) 8)
             (/ (if "nine" 1) 9)
             (number :d))
    :not-digit (if-not :digit 1)
    :main (any (choice :digit :not-digit))})

(comment
  (peg/match grammar "one1two")
  )

(with [f (file/open #"input/day-01-sample.txt"
                    "input/day-01-input.txt"
                    )]
  (->> (seq [line :in (file/lines f)]
         (peg/match grammar-part-2 line))
       (map |(+ (* 10 (first $)) (last $)))
       sum))
