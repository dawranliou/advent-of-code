(defpackage #:day-09
  (:use #:cl #:adventofcode))

(in-package #:day-09)

(defparameter +input+
  (uiop:read-file-lines
   (asdf:system-relative-pathname :adventofcode
                                  "src/day-09-input.txt")))

(defun parse-line (line)
  (let ((move (subseq line 0 1))
        (count (subseq line 2)))
    (list move (parse-integer count))))

;; (mapcar #'parse-line +input+)

(defun expand-input (input)
  (loop for (move count) in (mapcar #'parse-line input)
        nconcing (loop repeat count
                       collect move)))

(defparameter +flat-instructions+ (expand-input +input+))

(defun move (h-pos t-pos move)
  (let* ((h-next (list (+ (car h-pos) (car move))
                       (+ (cadr h-pos) (cadr move))))
         (tension (list (- (car h-next) (car t-pos))
                        (- (cadr h-next) (cadr t-pos))))
         (no-space-p (<= (+ (* (car tension) (car tension))
                            (* (cadr tension) (cadr tension)))
                         2))
         (t-next (list (+ (car t-pos)
                          (cond
                            (no-space-p 0)
                            ((< (car tension) 0) -1)
                            ((< 0 (car tension)) +1)
                            (t 0)))
                       (+ (cadr t-pos)
                          (cond
                            (no-space-p 0)
                            ((< (cadr tension) 0) -1)
                            ((< 0 (cadr tension)) +1)
                            (t 0))))))
    (list h-next t-next)))

;; (move '(1 1) '(0 0) '(0 1))

(defun part-1 ()
  (loop for instruction in +flat-instructions+
        for move = (cond
                     ((equal instruction "U") '(0 1))
                     ((equal instruction "D") '(0 -1))
                     ((equal instruction "L") '(-1 0))
                     ((equal instruction "R") '(1 0)))
        with h-trail = '((0 0))
        with t-trail = '((0 0))
        do (let ((h-pos (car h-trail))
                 (t-pos (car t-trail)))
             (destructuring-bind (h-new t-new) (move h-pos t-pos move)
               (push h-new h-trail)
               (push t-new t-trail)))
        finally (return (length (remove-duplicates t-trail :test #'equal)))))

;; (part-1) => 6090
