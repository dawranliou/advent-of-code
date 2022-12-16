(defpackage #:adventofcode
  (:use #:cl)
  (:export #:partition
           #:range
           #:split
           #:transpose
           #:repeat))

(in-package #:adventofcode)

(defun partition (n coll)
  (cond
    ((null coll) nil)
    ((< (length coll) n) (cons coll nil))
    (t (cons (subseq coll 0 n) (partition n (subseq coll n))))))

;; (partition 3 '(1 2 3 4 5 6 7 8 9 10))
;; (partition 3 '(1 2 3 4 5 6 7 8 9))

(defun range (max &key (min 0) (step 1))
  (loop for n from min below max by step
        collect n))

;; (range 10)
;; (range 10 :min 1 :step 2)

(setf (fdefinition 'split) #'uiop:split-string)

;; (split "1 2 3 4")
;; (split "1-2-3-4" :separator "-")

(defun transpose (seq)
  (loop for i from 0 below (length (first seq))
        collect (loop for s in seq
                      when (< i (length s))
                      collect (elt s i))))

;; (transpose '("1234" "5678"))
;; (transpose '("1234" "56"))

(defun repeat (thing n)
  (loop repeat n collect thing))

;; (repeat :hi 10)
;; (repeat '(0 0) 10)
