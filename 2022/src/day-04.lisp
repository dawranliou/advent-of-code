(defpackage #:day-04
  (:use #:cl #:adventofcode))

(in-package #:day-04)

(defparameter +input+
  (mapcar (lambda (line)
            (multiple-value-bind (match strings)
                (ppcre:scan-to-strings "(\\d+)-(\\d+),(\\d+)-(\\d+)" line)
              (assert match)
              (list
               (list (parse-integer (aref strings 0))
                     (parse-integer (aref strings 1)))
               (list (parse-integer (aref strings 2))
                     (parse-integer (aref strings 3))))))
          (uiop:read-file-lines
           (asdf:system-relative-pathname :adventofcode
                                          "src/day-04-input.txt"))))

;; (ppcre:scan-to-strings "(\\d+)-(\\d+),(\\d+)-(\\d+)" "57-93,9-57")

(defun expand-pair (pair)
  (destructuring-bind ((a1 a2) (b1 b2)) pair
    (list
     (range (1+ a2) :min a1)
     (range (1+ b2) :min b1))))

;; (expand-pair '((2 4) (6 8)))

(defun fully-contained-p (pair)
  (destructuring-bind (a b) pair
    (let ((a-len (length a))
          (b-len (length b))
          (overlapped-len (length (intersection a b))))
      (or (= a-len overlapped-len)
          (= b-len overlapped-len)))))

;; (fully-contained-p '((6) (4 5 6)))
;; (fully-contained-p '((6 7) (4 5 6)))


(defun part-1 ()
  (loop for pair in +input+
        count (fully-contained-p (expand-pair pair))))

;; (part-1) => 471

(defun overlap (pair)
  (destructuring-bind (a b) pair
    (intersection a b)))

;; (overlap '((5 6 7) (8 9)))

(defun part-2 ()
  (loop for pair in +input+
        for pair* = (expand-pair pair)
        count (overlap pair*)))

;; (part-2) => 888
