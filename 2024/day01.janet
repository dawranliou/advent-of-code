(def example `3   4
4   3
2   5
1   3
3   9
3   3
`)

(def grammar
  '{:main (some :line)
    :end (+ "\n" -1)
    :line (group (* (number :d+) (some :s) (number :d+) :end))})

(comment
  (peg/match grammar example)
  )

(def input (slurp "day01.input"))

(def data (peg/match grammar input))

(defn total-distance [data]
  (->> data
       (apply map array)
       (map sort)
       (apply map array)
       (map (partial apply -))
       (map math/abs)
       (apply +)))

(print "Part 1: " (total-distance data)) # => 1646452

(defn similarity [data]
  (let [[l1 l2] (apply map array data)
        occurs (frequencies l2)]
    (->> (map (juxt identity occurs) l1)
         (filter 1)
         (map (partial apply *))
         (apply +))))

(print "Part 2: " (similarity data)) # => 23609874
