(ns dawranliou.advent-of-code-2020.day-19
  (:require [clojure.string :as str]
            [clojure.walk :as w]
            [instaparse.core :as insta]))

(def input (slurp "inputs/day-19.txt"))

(def rules
  (->> (str/split input #"\n\n")
       first
       str/split-lines
       (sort-by #(Integer/parseInt (second (re-find #"(\d+): " %))))
       (str/join "\n")
       insta/parser))

(->> (str/split input #"\n\n")
     second
     str/split-lines
     (map rules)
     (remove insta/failure?)
     count)
