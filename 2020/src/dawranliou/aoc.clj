(ns dawranliou.aoc
  (:require [clojure.java.io :as io]))

(defn with-input [file f]
  (with-open [input (io/reader (str "inputs/" file))]
    (f input)))

(defn with-line [file map-fn f]
  (with-input file
    #(f (map map-fn (line-seq %)))))
