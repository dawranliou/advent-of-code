(defpackage #:day-05
  (:use #:cl #:adventofcode))

(in-package #:day-05)

(defparameter +input+
  (uiop:read-file-lines
   (asdf:system-relative-pathname :adventofcode
                                  "src/day-05-input.txt")))

(defun make-stacks ()
  (let ((layers (butlast (car (split-sequence:split-sequence
                               "" +input+ :test #'string=)))))
    (mapcar
     (lambda (stack)
       (mapcar
        (lambda (s) (elt s 1))
        (remove-if (lambda (s) (string= s "[_]")) stack)))
     (transpose
      (loop for layer in layers
            collect (str:split " "
                               (str:replace-all
                                "    "
                                " [_]"
                                layer)))))))

(defparameter *stacks* (make-stacks))

;; *stacks*
;; (let ((l '(1 2 3 4 5))) (pop l) l)

(defun get-top-of-stacks ()
  (loop for s in *stacks*
        collect (car s)))

;; (get-top-of-stacks)

(defun parse-instruction (line)
  (ppcre:register-groups-bind ((#'parse-integer count from to))
      ("move (\\d+) from (\\d+) to (\\d+)" line)
    (list count from to)))

(defparameter +instructions+
  (cadr (split-sequence:split-sequence "" +input+ :test #'string=)))

(defun part-1 ()
  (let ((*stacks* (make-stacks)))
    (loop for instruction in +instructions+
          do (destructuring-bind (count from to)
                 (parse-instruction instruction)
               (loop for i from 0 below count
                     do (push (pop (elt *stacks* (1- from)))
                              (elt *stacks* (1- to))))))
    (coerce (get-top-of-stacks) 'string)))

;; (part-1) => "RNZLFZSJH"


(defun part-2 ()
  (let ((*stacks* (make-stacks)))
    (loop for instruction in +instructions+
          do (destructuring-bind (count from to)
                 (parse-instruction instruction)
               (let ((acc))
                 (loop for i from 0 below count
                       do (push (pop (elt *stacks* (1- from))) acc))
                 (loop for i from 0 below count
                       do (push (pop acc) (elt *stacks* (1- to)))))))
    (coerce (get-top-of-stacks) 'string)))

;; (part-2) => "CNSFCGJSM"
