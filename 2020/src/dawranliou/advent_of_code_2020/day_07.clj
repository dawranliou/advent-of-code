(ns dawranliou.advent-of-code-2020.day-07
  (:require [dawranliou.aoc :as aoc]
            [clojure.string :as s]))

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
  (let [[_ desc color content]
        (re-matches #"(\w+) (\w+) bags contain (.+)\." s)

        children (when-not (= "no other bags" content)
                   (->> (s/split content #", ")
                        (mapv (fn [child]
                                (-> (zipmap [:amount :desc :color]
                                            (rest (re-find #"(\d+) (\w+) (\w+)" child)))
                                    (update :amount #(Integer/parseInt %)))))))]
    {:desc desc
     :color color
     :children children}))

(first
  (map parse (s/split test-input #"\n")))
;; => {:desc "light",
;;     :color "red",
;;     :children
;;     [{:amount 1, :desc "bright", :color "white"}
;;      {:amount 2, :desc "muted", :color "yellow"}]}

(defn bag->id [bag]
  (select-keys bag [:desc :color]))

(->> (s/split test-input #"\n")
     (map parse)
     first
     bag->id)
;; => {:desc "light", :color "red"}

(defn parent? [child-id maybe-parent]
  (->> (:children maybe-parent)
       (some #(= child-id (bag->id %)))))

(parent? {:desc "bright" :color "white"}
         {:desc "faded" :color "yellow"
          :children [{:amount 1 :desc "bright" :color "white"}
                     {:amount 2 :desc "muted" :color "yellow"}]})
;; => true

(defn direct-parents [bags target-id]
  (->> bags
       (keep #(when (parent? target-id %)
               (bag->id %)))
       vec))

(direct-parents [{:desc 1 :color 1 :children [{:desc 2 :color 2}]}]
                {:desc 2 :color 2})
;; => [{:desc 1, :color 1}]

(defn index [bags]
  (->> bags
       (map #(let [bag-id (bag->id %)]
               [bag-id (assoc % :parents (direct-parents bags bag-id))]))
       (into {})))

(def bags
  (aoc/with-line "day-07.txt" parse index))

(defn all-parents [target-id]
  (let [{:keys [parents]} (get bags target-id)]
    (concat parents (mapcat all-parents parents))))

;; part 1
(->>
  (all-parents {:desc "shiny" :color "gold"})
  distinct
  count)

(defn total-bags [target-id]
  (let [{:keys [children]} (get bags (bag->id target-id))]
    (map
      #(* (:amount %) (reduce + 1 (total-bags %)))
      children)))

;; part 2
(->> (total-bags {:desc "shiny" :color "gold"})
     (reduce +))
;; => 27526
