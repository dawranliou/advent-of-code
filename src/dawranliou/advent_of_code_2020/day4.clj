(ns dawranliou.advent-of-code-2020.day4
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def input (slurp "inputs/2020/day4.txt"))

(defn reshape [s]
  (->> (s/split s #" ")
       (map #(s/split % #":"))
       (into {})))

#_
(reshape "a:1 b:2")

(def required-entries
  #{"byr"
    "iyr"
    "eyr"
    "hgt"
    "hcl"
    "ecl"
    "pid"})

(defn has-all-required? [m]
  (every? #(m %) required-entries))

#_
(has-all-required? {"byr" 1
                    "iyr" 1
                    "eyr" 1
                    "hgt" 1
                    "hcl" 1
                    "ecl" 1
                    "pid" 1})

(comment

  ;; test input
  (def input "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in")

  ;; part 1
  (->>
    (s/split input #"\n\n")
    (map #(s/replace % #"\n" " "))
    (map reshape)
    (filter has-all-required?)
    count)
  ;; => 210

  )

(def validate-fn
  {"byr" #(when-let [digits (re-matches #"\d{4}" %)]
            (<= 1920 (Integer/parseInt digits) 2002))
   "iyr" #(when-let [digits (re-matches #"\d{4}" %)]
            (<= 2010 (Integer/parseInt digits) 2020))
   "eyr" #(when-let [digits (re-matches #"\d{4}" %)]
            (<= 2020 (Integer/parseInt digits) 2030))
   "hgt" #(when-let [[_ digits unit] (re-find #"(\d+)(in|cm)" %)]
            (case unit
              "cm" (<= 150 (Integer/parseInt digits) 193)
              "in" (<= 59 (Integer/parseInt digits) 76)))
   "hcl" #(re-matches #"#[0-9a-f]{6}" %)
   "ecl" #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"}
   "pid" #(re-matches #"[0-9]{9}" %)
   "cid" (constantly true)})

#_
((validate-fn "hgt") "180cm")

#_
(re-find #"(\d+)(in|cm)" "180cm")

#_
(re-find #"#[0-9a-f]+" "aaag13")

(defn valid-entry? [m]
  (every? (fn [[k v]] ((validate-fn k) v)) m))

#_
(valid-entry? {"byr" "1999"
               "iyr" "2011"
               "eyr" "2023"
               "hgt" "179cm"
               "hcl" "#aaaf"
               "ecl" "blu"
               "pid" "012345678"})

#_
(valid-entry? {"pid" "067285985",
               "hcl" "#ceb3a1",
               "cid" "281",
               "ecl" "#07219a",
               "eyr" "1944",
               "iyr" "2025",
               "byr" "2029",
               "hgt" "64cm"})

(comment
  ;; part 2
  (->>
    (s/split input #"\n\n")
    (map #(s/replace % #"\n" " "))
    (map reshape)
    (filter has-all-required?)
    (filter valid-entry?)
    count)
;; => 131
  )
