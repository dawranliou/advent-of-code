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

(def part-1
  (do
    (var head "AAA")
    (var step 0)
    (while (not= head "ZZZ")
      (let [i (% step (length instruction))
            move (get instruction i)
            target (get-in graph [head move])]
        (set head target)
        (++ step)))
    step))
