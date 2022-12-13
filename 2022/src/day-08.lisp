(defpackage #:day-08
  (:use #:cl #:adventofcode))

(in-package #:day-08)

(defparameter +input+
  (uiop:read-file-lines
   (asdf:system-relative-pathname :adventofcode
                                  "src/day-08-input.txt")))

(defun parse-line (line)
  (mapcar (lambda (char) (parse-integer (string char))) (coerce line 'list)))

(defparameter grid (mapcar #'parse-line +input+))

(defun part-1 ()
  (let ((grid (mapcar #'parse-line +input+))
        (acc))
    (loop for y from 0 below (length grid)
          for row = (elt grid y)
          with y-min = 0
          with y-max = (1- (length grid))
          do (loop for x from 0 below (length row)
                   for current-tree = (elt row x)
                   with x-min = 0
                   with x-max = (1- (length row))
                   when (or
                         ;; Edges
                         (<= y y-min)
                         (<= y-max y)
                         (<= x x-min)
                         (<= x-max x)
                         ;; up
                         (loop for y* from (1- y) downto y-min
                               always (< (elt (elt grid y*) x) current-tree))
                         ;; down
                         (loop for y* from (1+ y) to y-max
                               always (< (elt (elt grid y*) x) current-tree))
                         ;; left
                         (loop for x* from (1- x) downto x-min
                               always (< (elt (elt grid y) x*) current-tree))
                         ;; right
                         (loop for x* from (1+ x) to x-max
                               always (< (elt (elt grid y) x*) current-tree)))
                   do (push (list y x) acc))
          finally (return acc))
    (length acc)))

;; (part-1) => 1669

(defun part-2 ()
  (loop for y from 0 below (length grid)
        for row = (elt grid y)
        with y-min = 0
        with y-max = (1- (length grid))
        maximize
        ;; collect
        (loop for x from 0 below (length row)
              for current-tree = (elt row x)
              with x-min = 0
              with x-max = (1- (length row))
              maximize
              ;; collect
              (*
               ;; up
               (or (loop for y* from (1- y) downto y-min
                         for up-trees from 1
                         when (<= current-tree (elt (elt grid y*) x))
                           return up-trees
                         finally (return up-trees))
                   0)
               ;; down
               (or (loop for y* from (1+ y) to y-max
                         for down-trees from 1
                         when (<= current-tree (elt (elt grid y*) x))
                           return down-trees
                         finally (return down-trees))
                   0)
               ;; left
               (or (loop for x* from (1- x) downto x-min
                         for left-trees from 1
                         when (<= current-tree (elt (elt grid y) x*))
                           return left-trees
                         finally (return left-trees))
                   0)
               ;; right
               (or (loop for x* from (1+ x) to x-max
                         for right-trees from 1
                         when (<= current-tree (elt (elt grid y) x*))
                           return right-trees
                         finally (return right-trees))
                   0)))))

;; (part-2) => 331344
