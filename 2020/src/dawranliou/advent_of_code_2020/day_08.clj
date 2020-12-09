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
       (map-indexed (fn [idx {:keys [ins arg] :as itm}]
                      (let [nxt (if (= ins "jmp")
                                  (+ idx arg)
                                  (inc idx))]
                        (assoc itm :idx idx :nxt nxt))))
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
;; => [{:ins "nop", :arg 0, :idx 0, :nxt 1}
;;     {:ins "acc", :arg 1, :idx 1, :nxt 2}
;;     {:ins "jmp", :arg 4, :idx 2, :nxt 6}
;;     {:ins "acc", :arg 3, :idx 3, :nxt 4}
;;     {:ins "jmp", :arg -3, :idx 4, :nxt 1}
;;     {:ins "acc", :arg -99, :idx 5, :nxt 6}
;;     {:ins "acc", :arg 1, :idx 6, :nxt 7}
;;     {:ins "jmp", :arg -4, :idx 7, :nxt 3}
;;     {:ins "acc", :arg 6, :idx 8, :nxt 9}]

(defn last-acc-value-before-inf-loop [ops]
  (loop [{:keys [ins arg nxt] :as op} (first ops)
         acc                          0
         visited?                     #{}]
    (cond
      ;; Reached the last op. No loop detected.
      (nil? op)     nil
      (visited? op) acc
      :else         (recur (nth ops nxt)
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
(->>
  (aoc/with-line "day-08.txt" parse index)
  last-acc-value-before-inf-loop)
;; => 1384
