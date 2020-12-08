(ns dawranliou.advent-of-code-2020.day-07
  (:require [dawranliou.aoc :as aoc]
            [clojure.string :as s]
            [clojure.walk :as w]))

(def test-input
  "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.")

(defn parse [s]
  (def -s s)
  (let [[_ container contains] (re-matches #"(.+) bags contain (.+)" s)
        contents (-> contains
                     (s/replace "." "")
                     (s/split #", "))]
    (if (= contents ["no other bags"])
      [container nil]
      [container (some->> contents
                          (map #(re-find #"(\d+) (.+) bag" %))
                          (map rest)
                          (map reverse)
                          (mapcat (fn [[k v]] [k (read-string v)]))
                          (apply hash-map))])))

(mapcat parse (s/split test-input #"\n"))
;; => (["light red" {"muted yellow" 2, "bright white" 1}]
;;     ["dark orange" {"muted yellow" 4, "bright white" 3}]
;;     ["bright white" {"shiny gold" 1}]
;;     ["muted yellow" {"shiny gold" 2, "faded blue" 9}]
;;     ["shiny gold" {"vibrant plum" 2, "dark olive" 1}]
;;     ["dark olive" {"dotted black" 4, "faded blue" 3}]
;;     ["vibrant plum" {"dotted black" 6, "faded blue" 5}]
;;     ["faded blue" nil]
;;     ["dotted black" nil])

(def parsed-test-input
  (apply hash-map
         (mapcat parse (s/split test-input #"\n"))))
;; => {"muted yellow" {"shiny gold" 2, "faded blue" 9},
;;     "light red" {"muted yellow" 2, "bright white" 1},
;;     "dotted black" nil,
;;     "dark orange" {"muted yellow" 4, "bright white" 3},
;;     "bright white" {"shiny gold" 1},
;;     "shiny gold" {"vibrant plum" 2, "dark olive" 1},
;;     "faded blue" nil,
;;     "vibrant plum" {"dotted black" 6, "faded blue" 5},
;;     "dark olive" {"dotted black" 4, "faded blue" 3}}

(def parsed-input
  (aoc/with-line "day-07.txt" parse (comp #(apply hash-map %) #(apply concat %))))

(def test-container-of
  (->> parsed-test-input
       (mapcat (fn [[k1 m]]
                 (for [[k2 c] m]
                   [k2 k1])))
       (reduce (fn [m [k v]]
                 (if (m k)
                   (update m k conj v)
                   (assoc m k #{v})))
               {})))
;; => {"shiny gold" #{"muted yellow" "bright white"},
;;     "faded blue" #{"muted yellow" "vibrant plum" "dark olive"},
;;     "muted yellow" #{"light red" "dark orange"},
;;     "bright white" #{"light red" "dark orange"},
;;     "vibrant plum" #{"shiny gold"},
;;     "dark olive" #{"shiny gold"},
;;     "dotted black" #{"vibrant plum" "dark olive"}}

(loop [to-search ["shiny gold"]
       result #{}]
  (if (seq to-search)
    (let [containers (test-container-of (first to-search))]
      (recur (into (rest to-search) containers) (into result containers)))
    result))
;; => #{"muted yellow" "light red" "dark orange" "bright white"}

(def container-of
  (->> parsed-input
       (mapcat (fn [[k1 m]]
                 (for [[k2 c] m]
                   [k2 k1])))
       (reduce (fn [m [k v]]
                 (if (m k)
                   (update m k conj v)
                   (assoc m k #{v})))
               {})))

(count
  (loop [to-search ["shiny gold"]
         result #{}]
    (if (seq to-search)
      (let [containers (container-of (first to-search))]
        (recur (into (rest to-search) containers) (into result containers)))
      result)))
;; => 378
