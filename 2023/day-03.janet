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

(def engine-schema
  (peg/match engine-schema-grammar
             # sample
             (slurp "input/day-03-input.txt")))

(def symbols @{})

(loop [item :in engine-schema
       :let [{:row r :col c :sym ?sym :num ?num} item]]
  (put-in symbols [r c] item))

# Part 1
(-> (seq [{:row r :col c :num num} :in engine-schema
          :when num
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
(def numbers @{})

(loop [item :in engine-schema
       :let [{:row r :col c :num num} item]
       :when num
       :let [num-length (length (string num))]
       # Expand the number to occupy the dig-spanning spaces.  E.g. a 3-digit
       # number '123' at (row, col) of (10, 20) will be expanded to ocupy (10,
       # 20), (10, 21), and (10, 22)
       c-offset :range [0 num-length]]
  (put-in numbers [r (+ c c-offset)] item))

(-> (seq [{:row r :col c :sym sym} :in engine-schema
          :when sym
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
