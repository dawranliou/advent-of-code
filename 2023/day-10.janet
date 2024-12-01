(def grammar
  ~{:main (some :pipe)
    :pipe (<- (set "-|LJ7F.S"))})

(def parsed
  (with [f (file/open "input/day-10-sample-1.txt")]
    (seq [l :in (file/lines f)]
      (peg/match grammar l))))

(defn start-pos [graph]
  (prompt :pos
    (loop [[i row] :in (pairs graph)
           [j cell] :in (pairs row)]
      (when (= cell "S")
        (return :pos [i j])))))

(def graph
  (tabseq [[i row] :in (pairs parsed)
           [j cell] :in (pairs row)]
    [i j] (match cell
            "-" [[(dec i) j] [(inc i) j]]
            "|" [[i (dec j)] [i (inc j)]]
            "L" [[i (dec j)] [(inc i) j]]
            "J" [[i (dec j)] [(dec i) j]]
            "7" [[(dec i) j] [i (dec j)]]
            "F" [[(inc i) j] [i (inc j)]]
            "S" :start)))
