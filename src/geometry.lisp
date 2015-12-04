;;;; geometry.lisp

(in-package #:sketch)

;;;   ____ _____ ___  __  __ _____ _____ ______   __
;;;  / ___| ____/ _ \|  \/  | ____|_   _|  _ \ \ / /
;;; | |  _|  _|| | | | |\/| |  _|   | | | |_) \ V /
;;; | |_| | |__| |_| | |  | | |___  | | |  _ < | |
;;;  \____|_____\___/|_|  |_|_____| |_| |_| \_\|_|

(defun edges (vertices)
  (loop
     for i in (append (last vertices) (butlast vertices))
     for j in vertices
     collect (list i j)))

(defmacro with-lines (lines &body body)
  (flet ((i-to-s (i) (format nil "~a" i)))
    `(symbol-macrolet
	 ,(loop
	     for line in lines
	     for i upfrom 0 by 2
	     append
	       (loop
		  for sym in '(x x y y)
		  for idx in '(1 2 1 2)
		  for line-accessor in '(caar caadr cadar cadadr)
		  collect
		    `(,(alexandria:symbolicate sym (i-to-s (+ i idx)))
		       (,line-accessor ,line))))
       ,@body)))

(defun translate-line (line d)
  (with-lines (line)
    (let* ((a (atan (- y2 y1) (- x2 x1)))
	   (dx (* (sin a) d))
	   (dy (* (cos a) d)))
      `((,(+ x1 dx) ,(- y1 dy)) (,(+ x2 dx) ,(- y2 dy))))))

(defun intersect-lines (line1 line2)
  ;; https://en.wikipedia.org/wiki/Line–line_intersection#Given_two_points_on_each_line
  ;; The algorithm is changed so that division by zero never happens.
  ;; The values that are returned for "intersection" may or may not make sense, but
  ;; having responsive but wrong sketch is much better than a red screen.
  (with-lines (line1 line2)
    (let* ((denominator (- (* (- x1 x2) (- y3 y4))
			   (* (- y1 y2) (- x3 x4))))
	   (a (if (zerop denominator)
		  (/ (+ x2 x3) 2)
		  (/ (- (* (- (* x1 y2) (* y1 x2)) (- x3 x4))
			(* (- (* x3 y4) (* y3 x4)) (- x1 x2)))
		     denominator)))
	   (b (if (zerop denominator)
		  (/ (+ y2 y3) 2)
		  (/ (- (* (- (* x1 y2) (* y1 x2)) (- y3 y4))
			(* (- (* x3 y4) (* y3 x4)) (- y1 y2)))
		     denominator))))
      `(,a ,b))))

(defun grow-polygon (polygon d)
  (let ((poly
	 (mapcar (lambda (x) (apply #'intersect-lines x))
		 (edges (mapcar (lambda (x) (translate-line x (- d)))
				(edges polygon))))))
    (append (cdr poly) (list (car poly)))))
