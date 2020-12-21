(ns dawranliou.advent-of-code-2020.day-21
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [dawranliou.aoc :as aoc]))

(defn parse [l]
  (let [[_ ingredients allergens]
        (re-matches #"(.+) \(contains (.+)\)" l)]
    {:ingredients (set (str/split ingredients #"\s"))
     :allergens (set (str/split allergens #", "))}))

(def foods
  (aoc/with-line "day-21.txt" parse vec))

#_
(def foods
  (->> "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)"
       str/split-lines
       (mapv parse)))

(def all-allergens
  (apply set/union (map :allergens foods)))

(def food-grouped-by-allergen
  (into {}
        (for [allergen all-allergens]
          [allergen
           (filterv #(some (partial = allergen) (:allergens %)) foods)])))

(def allergen-ingredients
  (into {}
        (for [[allergen foods] food-grouped-by-allergen]
          (let [possible-ingredients
                (apply set/intersection (map :ingredients foods))
                impossible-ingredients
                (->> (map :ingredients foods)
                     (map #(set/difference % possible-ingredients))
                     (apply set/union))]
            [allergen
             {:possible   possible-ingredients
              :impossible impossible-ingredients}]))))

(def impossible-ingredients
  (let [possibles (->> allergen-ingredients
                       (map second)
                       (map :possible)
                       (apply set/union))]
    (->> (map #(set/difference % possibles) (->> allergen-ingredients
                                                 (map second)
                                                 (map :impossible)))
         (apply set/union))))

;; part 1
(->> foods
     (map :ingredients)
     (map #(filter impossible-ingredients %))
     (map count)
     (reduce +))
;; => 2078
