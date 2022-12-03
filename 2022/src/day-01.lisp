(defpackage #:day-01
  (:use #:cl))

(in-package #:day-01)

(defun read-input (filename)
  (with-open-file (f filename :direction :input)
    (split-sequence:split-sequence
     'line-break
     (loop for line = (read-line f nil nil)
           while line
           collect (if (string= "" line)
                       'line-break
                       (parse-integer line))))))

(defparameter +input+
  (read-input
   (asdf:system-relative-pathname
    :adventofcode
    "src/day-01-input.txt")))

(defun part-1 ()
  (loop for elf in +input+
        maximize (loop for calories in elf sum calories)))

(defun part-2 ()
  (reduce #'+
          (subseq
           (sort
            (loop for elf in +input+
                  collect (loop for calories in elf sum calories))
            #'>)
           0 3)))
