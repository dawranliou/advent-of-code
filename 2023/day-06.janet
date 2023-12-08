(def input
  (slurp #"input/day-06-sample.txt"
         "input/day-06-input.txt"))

(def grammar
  ~{:main (* (group :times) "\n" (group :distances) "\n")
    :times (* "Time:" (some (* :s+ (number :d+))))
    :distances (* "Distance:" (some (* :s+ (number :d+))))})

(def records
  (let [parsed (peg/match grammar input)
        [times distances] parsed]
    (map (fn [t d] {:time t :dist d}) times distances)))

(defn beat-record [{:time t-total :dist d-to-beat}]
  ``
  d_tobeat = f(t) = (t_total - t) * t = t* t_total - t^2
  => t^2 - t_total * t + d_tobeat = 0
  => quadratic formula with: a = 1, b = -t_total, c = d_tobeat
  ``
  (let [discriminant (math/sqrt (- (* t-total t-total) (* 4 d-to-beat)))
        min (/ (- t-total discriminant) 2.0)
        max (/ (+ t-total discriminant) 2.0)]
    (range (if (= min (math/ceil min))
             (inc min)
             (math/ceil min))
           (if (= max (math/floor max))
             max
             (inc (math/floor max))))))

(def part-1
  (product
    (seq [r :in records
          :let [times (beat-record r)]]
      (length times))))

(def part-2
  (length (beat-record #{:time 71530 :dist 940200}
                       {:time 40828492 :dist 233101111101487})))
