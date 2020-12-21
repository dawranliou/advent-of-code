(ns dawranliou.advent-of-code-2020.day-20
  (:require [clojure.string :as str]))

(def input (slurp "inputs/day-20.txt"))

(defn parse-tile [s]
  (let [lines (str/split-lines s)]
    {:id (->> (first lines)
              (re-matches #"Tile (\d+):")
              second
              read-string)
     :image (rest lines)}))

(def tiles
  (->> (str/split input #"\n\n")
       (map parse-tile)))

(defn edges [image]
  [(first image)
   (apply str (map first image))
   (last image)
   (apply str (map last image))])

(defn mirror [image]
  (mapv (comp (partial apply str) reverse) image))

(->> (first tiles)
     :image
     ((juxt identity mirror))
     (map edges))

(->> tiles
     (map #(assoc %
                  :edges (edges (:image %))
                  :edges-r (edges (mirror (:image %))))))
