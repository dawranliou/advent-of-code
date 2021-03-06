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

;; part 2
;; This is easy enough to manually figure out the right answer
(->> allergen-ingredients
     (map (fn [[k {:keys [possible]}]] [k possible])))
;; => (["eggs" #{"npxrdnd" "lmcqt" "ldkt" "kcddk"}]
;;     ["sesame" #{"fqpt" "lmcqt"}]
;;     ["peanuts" #{"cfb" "lmcqt" "ldkt"}]
;;     ["dairy" #{"lmcqt"}]
;;     ["shellfish" #{"cfb" "lmcqt" "jtfmtpd"}]
;;     ["soy" #{"fqpt" "lmcqt" "tsch"}]
;;     ["nuts" #{"cfb" "lmcqt"}]
;;     ["fish" #{"cfb" "npxrdnd" "lmcqt" "jtfmtpd"}])

;; lmcqt,kcddk,npxrdnd,cfb,ldkt,fqpt,jtfmtpd,tsch

;; part 2 programmatic solution
(def known-allergen
  (loop [allergen-ingredients (map (fn [[k {:keys [possible]}]]
                                     [k possible])
                                   allergen-ingredients)
         known-allergen       {}]
    (if (seq allergen-ingredients)
      (let [sorted-allergen-ingredients (sort-by (comp count second) allergen-ingredients)
            [allergen ingredients] (first sorted-allergen-ingredients)
            allergen-ingredient (first (remove known-allergen ingredients))]
        (recur
          (->> (rest sorted-allergen-ingredients)
               (map #(update-in % [1] (partial remove known-allergen))))
          (assoc known-allergen allergen-ingredient allergen)))
      known-allergen)))
;; => {"lmcqt" "dairy",
;;     "fqpt" "sesame",
;;     "cfb" "nuts",
;;     "tsch" "soy",
;;     "ldkt" "peanuts",
;;     "jtfmtpd" "shellfish",
;;     "npxrdnd" "fish",
;;     "kcddk" "eggs"}

(->> (sort-by second known-allergen)
     (map first)
     (str/join ","))
;; => "lmcqt,kcddk,npxrdnd,cfb,ldkt,fqpt,jtfmtpd,tsch"
