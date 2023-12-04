(def sample
  (slurp "input/day-03-sample.txt"))

(def engine-schema-grammar
  ~{:dot "."
    :number (/ (sequence (constant :row)
                         (line)
                         (constant :col)
                         (column)
                         (constant :num)
                         (number :d*))
               ,table)
    :symbol (/ (sequence (constant :row)
                         (line)
                         (constant :col)
                         (column)
                         (constant :sym)
                         (<- (if-not (choice :d :dot "\n") 1)))
               ,table)
    :line (some (choice :dot :number :symbol))
    :main (some (sequence :line "\n"))})

# Part 1
(let [input (peg/match engine-schema-grammar
                       # sample
                       (slurp "input/day-03-input.txt")
                       )]
  (def symbols @{})
  (loop [{:row r :col c :sym sym} :in input
         :when sym]
    (put-in symbols [r c] sym))

  # (pp symbols)

  (-> (seq [{:row r :col c :num num} :in input
            :when num
            :let [num-length (length (string num))
                  coords-to-check (seq [r-offset :range-to [-1 1]
                                        c-offset :range-to [-1 num-length]]
                                    [(+ r r-offset) (+ c c-offset)])]]
        (if (some |(get-in symbols $) coords-to-check)
          num
          0))
      sum))


# Part 2
(let [input (peg/match engine-schema-grammar
                       # sample
                       (slurp "input/day-03-input.txt")
                       )]
  (def numbers @{})
  (loop [item :in input
         :let [{:row r :col c :num num} item]
         :when num
         :let [num-length (length (string num))]
         c-offset :range [0 num-length]]
    (put-in numbers [r (+ c c-offset)] item))
  # (pp numbers)

  (-> (seq [{:row r :col c :sym sym} :in input
            :when sym
            :let [coords-to-check (seq [r-offset :range-to [-1 1]
                                        c-offset :range-to [-1 1]
                                        :unless (= [0 0] [r-offset c-offset])]
                                    [(+ r r-offset) (+ c c-offset)])
                  surrounding-nums (-> (keep |(get-in numbers $)
                                             coords-to-check)
                                       distinct)]
            :let [gear-ratio (if (= 2 (length surrounding-nums))
                               (* (get-in surrounding-nums [0 :num])
                                  (get-in surrounding-nums [1 :num]))
                               0)]]
        gear-ratio)
      sum))
