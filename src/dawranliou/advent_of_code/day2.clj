(ns dawranliou.advent-of-code.day2
  (:require [clojure.java.io :as io]))

(def input (io/resource "day-2-input.txt"))

(def line-regex
  #"(\d+)-(\d+) (\w): (\w+)")

(defn count-valid? [line min max letter pwd]
  (let [letter-count ((frequencies (map str pwd)) letter)]
    (<= min (or letter-count 0) max)))

(comment
  (with-open [rdr (io/reader input)]
    (->> (first (line-seq rdr))))
  ;; => "13-16 k: kkkkkgmkbvkkrskhd"

  (re-matches line-regex "13-16 k: kkkkkgmkbvkkrskhd")
  ;; => ["13-16 k: kkkkkgmkbvkkrskhd" "13" "16" "k" "kkkkkgmkbvkkrskhd"]

  (apply count-valid? ["13-16 k: kkkkkgmkbvkkrskhd" 13 16 "k" "kkkkkgmkbvkkrskhd"])
  ((frequencies (map str "kkkkkgmkbvkkrskhd")) "k")

  (with-open [rdr (io/reader input)]
    (->> (line-seq rdr)
         (map (partial re-matches line-regex))
         (map #(update % 1 read-string))
         (map #(update % 2 read-string))
         #_first
         (filter #(apply count-valid? %))
         vec
         count))
  ;; => 517
  )

(defn position-valid? [line pos1 pos2 letter pwd]
  (let [pos1-match? (= letter (str (get pwd (dec pos1))))
        pos2-match? (= letter (str (get pwd (dec pos2))))]
    (and (or pos1-match? pos2-match?)
         (not= pos1-match? pos2-match?))))

(comment

  (apply position-valid? ["1-2 k: k234k678" 1 2 "k" "k234k678"])

  (with-open [rdr (io/reader input)]
    (->> (line-seq rdr)
         (map (partial re-matches line-regex))
         (map #(update % 1 read-string))
         (map #(update % 2 read-string))
         #_first
         (filter #(apply position-valid? %))
         vec
         count))
  ;; => 284
  )
