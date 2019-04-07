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
               #:sdl2-image
               #:sdl2-ttf
               #:sdl2kit
               #:split-sequence
               #:static-vectors)
  :components ((:file "dump")))
