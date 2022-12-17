(defpackage #:day-10
  (:use #:cl #:adventofcode))

(in-package #:day-10)

(defparameter +input+
  (uiop:read-file-lines
   (asdf:system-relative-pathname :adventofcode
                                  "src/day-10-input.txt")))

(defparameter +cycles+ '((1 1)))
                         ; (cycle# X)

(defun parse-line (line)
  (if (equal line "noop")
    '((1 0))
    (list '(1 0)
          (list 1 (parse-integer (subseq line 5))))))

;; (parse-line "noop")
;; (parse-line "addx -12")

(defun v+ (v1 v2)
  (list (+ (car v1) (car v2))
        (+ (cadr v1) (cadr v2))))

;; (v+ '(3 5) '(1 2))

(defun part-1 ()
  (let ((cycles  '((1 1))))
    (loop for line in +input+
          do (loop for instruction in (parse-line line)
                   do (push (v+ instruction (car cycles)) cycles)))
    (loop for i from 20 to (length cycles) by 40
          with cycles = (reverse cycles)
          for (ncycle signal) = (nth (1- i) cycles)
          sum (* ncycle signal))))

;; (part-1)
