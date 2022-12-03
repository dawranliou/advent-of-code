(defpackage #:day-03
  (:use #:cl))

(in-package #:day-03)

(defconstant +input-filename+
  (asdf:system-relative-pathname :adventofcode "src/day-03-input.txt"))

(defun item-score (item-char)
  (let ((item-int (char-int item-char)))
    (if (< 96 item-int)
        (- item-int 96)
        (+ 26 (- item-int 64)))))

(defun parse-line (line)
  ;; (let ((line "vJrwpWtwJgWrhcsFMMfFFhFp"))
  (let* ((len (length line))
         (compartment-1 (coerce (subseq line 0 (/ len 2)) 'list))
         (compartment-2 (coerce (subseq line (/ len 2) len) 'list))
         (shared (car (intersection compartment-1 compartment-2))))
    (item-score shared)))

(defun part-1 ()
  (loop for line in (uiop:read-file-lines +input-filename+)
        sum (parse-line line)))
;; (part-1) => 7980

(defun partition-3 (input)
  (when input
    (destructuring-bind (a b c &rest rests) input
      (cons (list a b c) (partition-3 rests)))))

;; (partition-3 '(1 2 3 4 5 6 7 8 9))

(defun part-2 ()
  (loop for elves in (partition-3 (uiop:read-file-lines +input-filename+))
        for elves* = (mapcar (lambda (s) (coerce s 'list)) elves)
        sum (item-score (car (reduce #'intersection elves*)))))

;; (part-2) => 2881
