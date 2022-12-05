(defpackage #:day-04
  (:use #:cl #:adventofcode))

(in-package #:day-04)

(defun parse-line (line)
  (destructuring-bind (pair1 pair2) (split line :separator ",")
    (let ((range1 (split pair1 :separator "-"))
          (range2 (split pair2 :separator "-")))
      (list (mapcar #'parse-integer range1)
            (mapcar #'parse-integer range2)))))

(defparameter +input+
  (mapcar #'parse-line
          (uiop:read-file-lines
           (asdf:system-relative-pathname :adventofcode
                                          "src/day-04-input.txt"))))

(defun fully-contained-p (pairs)
  (destructuring-bind ((min1 max1) (min2 max2)) pairs
    (or (and (<= min1 min2) (<= max1 max2))
        (and (<= min2 min1) (<= max2 max1)))))

;; (fully-contained-p '((6 6) (4 6)))
;; (fully-contained-p '((6 7) (4 6)))

(defun part-1 ()
  (loop for pair in +input+
        count (fully-contained-p pair)))

;; (part-1) => 471

(defun overlapp (pairs)
  (destructuring-bind ((min1 max1) (min2 max2)) pairs
    (and (<= min1 max2) (<= min2 max1))))

;; (overlapp '((5 7) (8 9)))
;; (overlapp '((5 8) (8 9)))

(defun part-2 ()
  (loop for pairs in +input+
        count (overlapp pairs)))

;; (part-2) => 888
