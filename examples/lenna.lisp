;;;; lenna.lisp

(in-package #:sketch-examples)

(defsketch lenna
    ((title "lenna")
     (width 800)
     (height 600)
     (picture (load-resource (relative-path
			      "res/lenna.png"))))
  (image picture 0 0 304 304))
