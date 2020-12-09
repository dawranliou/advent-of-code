(ns dawranliou.advent-of-code-2020.day-08
  (:require [dawranliou.aoc :as aoc]
            [clojure.string :as s]))

(defn parse [s]
  (let [[_ ins arg] (re-matches #"(\w+) ([+-]\d+)" s)]
    {:ins ins
     :arg (Integer/parseInt arg)}))

(parse "nop +0")
;; => {:ins "nop", :arg 0}

(defn index [ops]
  (->> ops
       (map-indexed (fn [idx itm]
                      (assoc itm :idx idx)))
       vec))

(->> (s/split-lines "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6")
     (map parse)
     index)
;; => [{:ins "nop", :arg 0, :idx 0}
;;     {:ins "acc", :arg 1, :idx 1}
;;     {:ins "jmp", :arg 4, :idx 2}
;;     {:ins "acc", :arg 3, :idx 3}
;;     {:ins "jmp", :arg -3, :idx 4}
;;     {:ins "acc", :arg -99, :idx 5}
;;     {:ins "acc", :arg 1, :idx 6}
;;     {:ins "jmp", :arg -4, :idx 7}
;;     {:ins "acc", :arg 6, :idx 8}]

(defn last-acc-value-before-inf-loop [ops]
  (loop [{:keys [ins arg idx] :as op} (first ops)
         acc                          0
         visited?                     #{}]
    (cond
      ;; Reached the last op. No loop detected.
      (nil? op)     nil
      (visited? op) acc
      :else         (recur (nth ops (if (= ins "jmp")
                                      (+ idx arg)
                                      (inc idx)))
                           (if (= ins "acc")
                             (+ acc arg)
                             acc)
                           (conj visited? op)))))

(->> (s/split-lines "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6")
     (map parse)
     index
     last-acc-value-before-inf-loop)
;; => 5

;; part 1
(def ops (aoc/with-line "day-08.txt" parse index))

(->>
  ops
  last-acc-value-before-inf-loop)
;; => 1384

(defn termination-value [ops]
  (loop [{:keys [ins arg idx] :as op} (first ops)
         acc                          0
         visited?                     #{}]
    (cond
      ;; Reached the last op. No loop detected.
      (nil? op)     acc
      ;; Return nil for inf. loops
      (visited? op) nil
      :else         (recur (nth ops
                                (if (= ins "jmp")
                                  (+ idx arg)
                                  (inc idx))
                                nil)
                           (if (= ins "acc")
                             (+ acc arg)
                             acc)
                           (conj visited? op)))))

(->> (s/split-lines "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
nop -4
acc +6")
     (map parse)
     index
     termination-value)
;; => 8

(def jmp<->nop
  {"jmp" "nop"
   "nop" "jmp"
   "acc" "acc"})

;; Part 2
(->> ops
     (map-indexed (fn [idx op]
                    (update-in ops [idx :ins] jmp<->nop)))
     (keep termination-value))
;; => (761)
