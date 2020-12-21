(ns dawranliou.advent-of-code-2020.day-20
  (:require [clojure.string :as str]))

(def input (slurp "inputs/day-20.txt"))

(defn parse-tile [s]
  (let [lines (str/split-lines s)]
    {:id (->> (first lines)
              (re-matches #"Tile (\d+):")
              second
              read-string)
     :image (vec (rest lines))}))

(def tiles
  (->> (str/split input #"\n\n")
       (map parse-tile)))

(defn rotate [image]
  (->> image
       (apply map vector)
       (map reverse)
       (mapv (partial apply str))))

(defn flip [image]
  (mapv (comp (partial apply str) reverse) image))

(defn edges [image]
  {:top    (first image)
   :left   (apply str (map first image))
   :bottom (last image)
   :right  (apply str (map last image))})

(def opposite-edge
  {:top :bottom
   :left :right
   :bottom :top
   :right :left})
