(def line-grammar
  ~{:main (* (some (+ (number (* (? "-"):d+)) " ")) "\n")})

(def parsed
  (with [f (file/open #"input/day-09-sample.txt"
                      "input/day-09-input.txt")]
    (seq [l :in (file/lines f)]
      (peg/match line-grammar l))))

(defn diffs [nums]
  (map - (array ;(drop 1 nums)) nums))

(defn extrapolate [nums]
  (if (all zero? nums)
    0
    (+ (last nums) (extrapolate (diffs nums)))))

(def part-1
  (sum (seq [nums :in parsed]
         (extrapolate nums))))

(defn extrapolate- [nums]
  (if (all zero? nums)
    0
    (- (first nums) (extrapolate- (diffs nums)))))

(def part-2
  (sum (seq [nums :in parsed]
         (extrapolate- nums))))
