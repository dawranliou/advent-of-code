(asdf:defsystem #:adventofcode
  :depends-on
  ("split-sequence"
   "cl-ppcre"
   "str")

  :serial t
  :components
  ((:module "src"
    :components
    ((:file "core")))))
