(def engine-schema-grammar
  ~{:dot "."
    :number (group (sequence (line)
                             (column)
                             (constant :num)
                             (number :d+)))
    :symbol (group (sequence (line)
                             (column)
                             (constant :sym)
                             (<- (set "*+#%-/@=$&"))))
    :line (sequence (some (choice :dot :number :symbol)) "\n")
    :main (some :line)})

(def engine-schema
  (peg/match engine-schema-grammar
             # (slurp "input/day-03-sample.txt")
             (slurp "input/day-03-input.txt")
             ))

(def symbols "Sparse 2D array of symbols indexed by row then col" @{})
(def numbers "Sparse 2D array of numbers indexed by row then col" @{})

(loop [[id item] :pairs engine-schema
       :let [[row col type data] item
             data-length (length (string data))]
       # Expand the number to occupy the dig-spanning spaces.  E.g. a 3-digit
       # number '123' at (row, col) of (10, 20) will be expanded to ocupy (10,
       # 20), (10, 21), and (10, 22)
       col-offset :range [0 data-length]]
  (if (= type :num)
    (put-in numbers [row (+ col col-offset)] {:id id :num data})
    (put-in symbols [row (+ col col-offset)] {:id id :sym data})))

# Part 1
(-> (seq [[r c type num] :in engine-schema
          :when (= type :num)
          :let [num-length (length (string num))
                # Check all spaces surrounding (and including) the number
                coords-to-check (seq [r-offset :range-to [-1 1]
                                      c-offset :range-to [-1 num-length]]
                                  [(+ r r-offset) (+ c c-offset)])]]
      (if (some |(get-in symbols $) coords-to-check)
        num
        0))
    sum)

# Part 2
(-> (seq [[r c type sym] :in engine-schema
          :when (= type :sym)
          :let [coords-to-check (seq [r-offset :range-to [-1 1]
                                      c-offset :range-to [-1 1]
                                      # Check the 8 spaces around the symbol
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
    sum)
