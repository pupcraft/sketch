;;;; lenna.lisp

(in-package #:sketch-examples)

(defsketch lenna
    ((title "lenna")
     (width 256)
     (height 256)
     (picture (load-resource (relative-path
			      "res/lenna.png"))))
  (image picture 0 0 256 256)
  (text "What the fuck did you just fucking say about me, you little bitch?
 I'll have you know I graduated top of my class in the Navy Seals, 
and I've been involved in numerous secret raids on Al-Quaeda, 
and I have over 300 confirmed kills." 150 200 300 100))
