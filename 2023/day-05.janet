(def input (slurp "input/day-05-sample.txt"
                  # "input/day-05-input.txt"
                  ))

(def grammar
  ~{:main (sequence (some (choice :seeds "\n" :mapping)))
    :seeds (group (sequence "seeds:" (some (sequence :s (number :d+))) "\n"))
    :mapping (group (sequence (<- :entity) "-to-" (<- :entity) :s "map:\n"
                              (some (group (sequence (number :d+) :s
                                                     (number :d+) :s
                                                     (number :d+) "\n")))))
    :entity (choice "seed" "soil" "fertilizer" "water" "light"
                    "temperature" "humidity" "location")})

(def parsed-input
  (peg/match grammar input))

# This approach breaks Janet :(
# (def mappings "[from to] -> from number -> to number"
#   (seq [[_from _to & rules] :in (drop 1 parsed-input)]
#     (tabseq [[des-start src-start range-length] :in rules
#              offset :range [0 range-length]]
#       (+ src-start offset) (+ des-start offset))))
#
# (seq [seed :in (first parsed-input)]
#   (reduce (fn [current-num current-mapping]
#             (get current-mapping current-num current-num))
#           seed
#           mappings))

(defn src->dest [src rules]
  (if-let [[dest-start src-start _range-length]
           (some (fn [rule]
                   (let [[_dest-start src-start range-length] rule]
                     (when (<= src-start src (+ src-start range-length -1))
                       rule)))
                 rules)]
    (+ dest-start (- src src-start))
    src))

# Part 1
(let [[seeds & mappings] parsed-input]
  (min ;(seq [seed :in seeds]
          (reduce (fn [current-num [_from _to & rules]]
                    (src->dest current-num rules))
                  seed
                  mappings))))

# Part 2

# Bruteforce isn't performant enough
# (let [[seeds & mappings] parsed-input]
#   (min ;(seq [[seed-start seed-range] :in (partition 2 seeds)
#               seed :range [seed-start (+ seed-start seed-range)]]
#           (reduce (fn [current-num [_from _to & rules]]
#                     (src->dest current-num rules))
#                   seed
#                   mappings))))

(def [seeds & mappings] parsed-input)

(def seed-ranges
  (partition 2 seeds))

(defn second [x] (get x 1))

(defn overlap? [r1 r2]
  (let [[start-1 length-1] r1
        [start-2 length-2] r2]
    (or (<= start-1 start-2 (+ start-1 length-1 -1))
        (<= start-2 start-1 (+ start-2 length-2 -1)))))

(catseq [seed-range :in seed-ranges]
        (seq [[dest-start src-start range-length] :in mappings
              :when (overlap? seed-range [src-start range-length])]))
