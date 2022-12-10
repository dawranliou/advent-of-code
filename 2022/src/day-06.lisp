(defpackage #:day-06
  (:use #:cl #:adventofcode))

(in-package #:day-06)

(defparameter +input+
  (car (uiop:read-file-lines
        (asdf:system-relative-pathname :adventofcode
                                       "src/day-06-input.txt"))))

(defvar *input*)

(defun part-1 ()
  (loop for i from 0 below (- (length *input*) 4)
        for packet = (subseq *input* i (+ 4 i))
        when (= 4 (length (remove-duplicates packet)))
          return (+ i 3 1)))

#+nil
(let ((*input*
        ;; "bvwbjplbgvbhsrlpgdmjqwftvncz"
        ;; "nppdvjthqldpwncqszvftbrmjlhg"
        ;; "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
        ;;"zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
        ))

  (part-1) ;; 1578

  )

(defun part-2 ()
  (loop for i from 0 below (- (length *input*) 14)
        for packet = (subseq *input* i (+ 14 i))
        when (= 14 (length (remove-duplicates packet)))
          return (+ i 13 1)))
;; (part-2)  => 2178
