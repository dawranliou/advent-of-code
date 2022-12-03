(defpackage #:day-02
  (:use #:cl))

(in-package #:day-02)

(defconstant +input-filename+
  (asdf:system-relative-pathname :adventofcode "src/day-02-input.txt"))

(defparameter +input+
  (loop for line in (uiop:read-file-lines +input-filename+)
        collect (destructuring-bind (other me)
                    (split-sequence:split-sequence #\Space line)
                  (list (read-from-string other) (read-from-string me)))))

(defun score (round)
  (cond
    ((equal round '(A X)) (+ 3 1))
    ((equal round '(A Y)) (+ 6 2))
    ((equal round '(A Z)) (+ 0 3))
    ((equal round '(B X)) (+ 0 1))
    ((equal round '(B Y)) (+ 3 2))
    ((equal round '(B Z)) (+ 6 3))
    ((equal round '(C X)) (+ 6 1))
    ((equal round '(C Y)) (+ 0 2))
    ((equal round '(C Z)) (+ 3 3))))

(defun part-1 ()
  (loop for round in +input+
        sum (score round)))
;; (part-1) => 12586

;; Part 2
(defun score-2 (round)
  (cond
    ((equal round '(A X)) (+ 0 3))      ; Lose to rock
    ((equal round '(A Y)) (+ 3 1))      ; Draw to rock
    ((equal round '(A Z)) (+ 6 2))      ; Win to rock
    ((equal round '(B X)) (+ 0 1))      ; Lose to paper
    ((equal round '(B Y)) (+ 3 2))      ; Draw to paper
    ((equal round '(B Z)) (+ 6 3))      ; Win to paper
    ((equal round '(C X)) (+ 0 2))      ; Lose to scissors
    ((equal round '(C Y)) (+ 3 3))      ; Draw to scissors
    ((equal round '(C Z)) (+ 6 1))))    ; Win to scissors

(defun part-2 ()
  (loop for round in +input+
        sum (score-2 round)))
;; (part-2) => 13193
