(asdf:defsystem #:adventofcode
  :depends-on
  ("split-sequence")

  :serial t
  :components
  ((:module "src"
    :components
    ((:file "day-01")))))
