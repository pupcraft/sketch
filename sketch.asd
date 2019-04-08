;;;; sketch.asd

(asdf:defsystem #:sketch
  :description "Sketch is a Common Lisp framework for the creation of electronic art, computer graphics, visual design, game making and more. It is inspired by Processing and OpenFrameworks."
  :author "Danilo Vidovic (vydd)"
  :license "MIT"
  :depends-on (#:alexandria
               #:cl-geometry
               #:glkit
               #:mathkit
               #:md5
	       #:uncommon-lisp
	       #:opticl
               #:split-sequence
               #:static-vectors
	       #:glhelp
	       #:vecto)
  :components ((:file "util")
	       (:file "dump")))
