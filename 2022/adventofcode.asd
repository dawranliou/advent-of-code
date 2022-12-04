(asdf:defsystem #:adventofcode
  :depends-on
  ("split-sequence"
   "cl-ppcre")

  :serial t
  :components
  ((:module "src"
    :components
    ((:file "core")
     (:file "day-01")
     (:file "day-02")
     (:file "day-03")))))
