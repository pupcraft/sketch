;;;; sketch-examples.asd

(asdf:defsystem #:sketch-examples
  :description "Sketch examples"
  :author "Danilo Vidovic (vydd)"
  :license "MIT"
  :depends-on (#:alexandria
               #:sketch)
  :serial t
  :components ((:file "examples")))
