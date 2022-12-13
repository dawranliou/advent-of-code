(defpackage #:day-07
  (:use #:cl #:adventofcode))

(in-package #:day-07)

(defparameter +input+
  (uiop:read-file-lines
   (asdf:system-relative-pathname :adventofcode
                                  "src/day-07-input.txt")))

(defun make-dir-tree (lines)
  (loop with root = (make-hash-table)
        with path = (list root)
        finally (return root)
        for line in lines
        do (cond
             ((uiop:string-prefix-p "$ cd " line)
              (let ((dir-path (subseq line 5)))
                (cond
                  ((string= "/" dir-path) (setq path (list root)))
                  ((string= ".." dir-path) (pop path))
                  (t (let ((dir (make-hash-table)))
                       (setf (gethash dir-path (car path)) dir)
                       (push dir path))))))
             ((string= "$ ls" line) nil)
             ((uiop:string-prefix-p "dir " line) nil)
             (t (destructuring-bind (size-str filename) (uiop:split-string line)
                  (setf (gethash filename (car path))
                        (parse-integer size-str)))))))
(defparameter +tree+
  (make-dir-tree +input+))

(defparameter +sizes+ (make-hash-table :test #'equal))

(defun parse-size (acc cwd tree)
  (loop for k being the hash-key
          using (hash-value v) of tree
        for size = (if (hash-table-p v)
                       (parse-size acc (str:concat cwd "/" k) v)
                       v)
        sum size into total-size
        finally (progn (setf (gethash cwd acc) total-size)
                       (return total-size))))

;; (parse-size +sizes+ "" +tree+)
;; +sizes+

(defun part-1 ()
  (let ((file-system (make-dir-tree +input+))
        (flat-dir-sizes (make-hash-table)))
    (parse-size flat-dir-sizes "" file-system)
    (loop for dir being the hash-key
            using (hash-value dir-size) of flat-dir-sizes
          when (<= dir-size 100000)
            ;; collect (list dir dir-size))
            sum dir-size)))

;; (part-1) => 1642503

(defparameter +available-space+ 70000000)
(defparameter +required-free-space+ 30000000)
(defparameter +max-used-space+ (- +available-space+ +required-free-space+))

(defun part-2 ()
  (let ((total-used-space (gethash "" +sizes+))
        (sorted-dir-space (sort (loop for dir-size being the hash-value
                                        of +sizes+
                                      collect dir-size)
                                #'<)))
    (loop for dir-size in sorted-dir-space
          when (< (- total-used-space +max-used-space+) dir-size)
            return dir-size)))
;; (part-2) => 6999588
