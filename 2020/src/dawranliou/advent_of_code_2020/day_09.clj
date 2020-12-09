(ns dawranliou.advent-of-code-2020.day-09
  (:require [dawranliou.aoc :as aoc]
            [clojure.string :as s]))

(defn index [preamble-size numbers]
  (->> numbers
       (partition (inc preamble-size) 1)
       (map #(hash-map :preamble (vec (take preamble-size %))
                       :number   (last %)))))

(defn sum-of-any-two? [{:keys [preamble number]}]
  (not-empty
    (for [x     preamble
          y     preamble
          :when (and (not= x y)
                     (= number (+ x y)))]
      {:preamble #{x y}
       :number number})))

(->>
  [35 20 15 25 47 40 62 55 65 95 102 117 150 182 127 219 299 277 309 576]
  (index 5)
  (filter (complement sum-of-any-two?)))
;; => ({:number 127, :preamble [95 102 117 150 182]})

;; part 1
(def input
  (aoc/with-line "day-09.txt" read-string vec))

(->> input
     (index 25)
     (filter (complement sum-of-any-two?)))
;; => ({:number 1309761972,
;;      :preamble
;;      [897113874
;;       1091023444
;;       898424504
;;       937778548
;;       933761359
;;       944365859
;;       1571755268
;;       1038097719
;;       1044685030
;;       1054676249
;;       1184374544
;;       1432043299
;;       1139239203
;;       1647140981
;;       1936522223
;;       1199152059
;;       1226018331
;;       1233009817
;;       1247787332
;;       1978446389
;;       1315387016
;;       2253165564
;;       1483101955
;;       1536217310
;;       1677440707]})

;; part 2
(def weakness
  (->> input
       (index 25)
       (filter (complement sum-of-any-two?))
       first))

(->> (range 2 (inc 5))
     (mapcat #(partition % 1 [35 20 15 25 47 40 62 55 65 95 102 117 150 182 127 219 299 277 309 576]))
     (filter #(= 127 (reduce + %))))
;; => ((15 25 47 40))

(let [numbers
      (->> (range 2 (inc 25))
           (mapcat #(partition % 1 input))
           (filter #(= (:number weakness) (apply + %)))
           first)]
  (+ (apply min numbers)
     (apply max numbers)))
;; => 177989832
