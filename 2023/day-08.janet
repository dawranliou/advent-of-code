(def grammar
  ~{:main (some (+ :instruction "\n" :node))
    :instruction (* (group (some (<- (set "RL")))) "\n")
    :node (group (* (<- :w+) " = (" (<- :w+) ", " (<- :w+) ")\n"))})

(def [instruction & nodes]
  (peg/match grammar (slurp #"input/day-08-sample-1.txt"
                            #"input/day-08-sample-2.txt"
                            "input/day-08-input.txt"
                            )))

(def graph
  (tabseq [[head left right] :in nodes]
    head {"L" left "R" right}))

(defn steps [head end-condition?]
  (var head head)
  (var step 0)
  (while (not (end-condition? head))
    (let [i (% step (length instruction))
          move (get instruction i)
          target (get-in graph [head move])]
      (set head target)
      (++ step)))
  step)

(def part-1
  (steps "AAA" |(= $ "ZZZ")))

(defn head? [node]
  (= "A" (string/slice node 2)))

(defn end? [node]
  (= "Z" (string/slice node 2)))

(def part-2
  (->> (filter head? (keys graph))
       (map |(steps $ end?))
       (reduce2 math/lcm)))
