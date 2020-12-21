(ns dawranliou.advent-of-code-2020.day-19
  (:require [clojure.string :as str]
            [clojure.walk :as w]
            [instaparse.core :as insta]))

(def input (slurp "inputs/day-19.txt"))

(def sorted-input
  (->> (str/split input #"\n\n")
       first
       str/split-lines
       (sort-by #(Integer/parseInt (second (re-find #"(\d+): " %))))
       (str/join "\n")))

(def rules
  (insta/parser sorted-input))

(def examples
  (->> (str/split input #"\n\n")
       second
       str/split-lines))

(->> examples
     (map rules)
     (remove insta/failure?)
     count)

(def rules-2
  (-> sorted-input
      (str/replace #"\n8\: 42\n" "\n8: 42 | 42 8\n")
      (str/replace #"\n11\: 42 31\n" "\n11: 42 31 | 42 11 31\n")
      insta/parser))

(->> examples
     (map rules-2)
     (remove insta/failure?)
     count)
