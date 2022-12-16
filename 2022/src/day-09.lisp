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

(defun move-2 (knots move)
  (let* ((h-pos (car knots))
         (h-next (list (+ (car h-pos) (car move))
                       (+ (cadr h-pos) (cadr move)))))
    (setf (car knots) h-next))
  (loop for n from 1 below (length knots)
        for h-pos = (nth (1- n) knots)
        for t-pos = (nth n knots)
        do (let* ((tension (list (- (car h-pos) (car t-pos))
                                 (- (cadr h-pos) (cadr t-pos))))
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
             (setf (nth n knots) t-next))))

;; (let ((knots (repeat '(0 0) 10)))
;;   (move-2 knots '(0 1))
;;   knots)

(defun part-2 ()
  (loop for instruction in +flat-instructions+
        for move = (cond
                     ((equal instruction "U") '(0 1))
                     ((equal instruction "D") '(0 -1))
                     ((equal instruction "L") '(-1 0))
                     ((equal instruction "R") '(1 0)))
        with knots = (repeat '(0 0) 10)
        with t-trail = nil
        do (progn
             (move-2 knots move)
             (push (first (last knots)) t-trail))
        finally (return (length (remove-duplicates t-trail :test #'equal)))))

;; (part-2)=> 2566
