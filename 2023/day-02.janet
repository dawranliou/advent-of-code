# Part 1

(def sample
  ``
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
``)

(defn ->set-table [& args]
  (def table @{})
  (loop [[n color] :in (partition 2 args)]
    (put table color n))
  table)

(def game-grammar
  ~{:main (sequence :game (any :set))
    :game (sequence "Game " (number :d*) ": ")
    :set (/ (sequence (any :reveal) (? "; ")) ,->set-table)
    :reveal (sequence (number :d*) " " (capture :cube) (? ", "))
    :cube (choice "blue" "red" "green")})

(comment
  (peg/match game-grammar
             "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")
  )

(def max-cubes {"red" 12 "green" 13 "blue" 14})

(defn possible-set? [set-result]
  (all (fn [[color count]] (<= count (max-cubes color)))
       (pairs set-result)))

(comment
  (possible-set? {"red" 10 "green" 14})
  )

(with [f (file/open # "input/day-02-sample.txt"
                    "input/day-02-input.txt"
                    )]
  (->> (seq [line :in (file/lines f)]
         (peg/match game-grammar line))
       (filter (fn [[n & sets]]
                 (all possible-set? sets)))
       (map first)
       sum))

# Part 2
(defn min-possible-cubes [[n & sets]]
  {"red" (max ;(map |(get $ "red" 0) sets))
   "green" (max ;(map |(get $ "green" 0) sets))
   "blue" (max ;(map |(get $ "blue" 0) sets))})

(comment
  (min-possible-cubes [1
                       {"blue" 3 "red" 4}
                       {"red" 1 "green" 2 "blue" 6}
                       {"green" 2}])
  )

(with [f (file/open # "input/day-02-sample.txt"
                    "input/day-02-input.txt"
                    )]
  (->> (seq [line :in (file/lines f)]
         (-> (peg/match game-grammar line)
             min-possible-cubes
             values
             product))
       sum))
