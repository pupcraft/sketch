(defpackage :sketch-util
  (:use :cl))
(in-package :sketch-util)

(defun file-name-extension (name)
  ;; taken from dto's xelf code
  (let ((pos (position #\. name :from-end t)))
    (when (numberp pos)
      (subseq name (1+ pos)))))

(defun opticl-load-image (file)
  (let ((extension (file-name-extension file)))
    (cond ((string= extension "png")
	   (opticl:read-png-file file))
	  ((string= extension "jpeg")
	   (opticl:read-jpeg-file file))
	  ((string= extension "tiff")
	   (opticl:read-tiff-file file))
	  ((string= extension "pnm")
	   (opticl:read-pnm-file file))
	  ((string= extension "pbm")
	   (opticl:read-pbm-file file))
	  ((string= extension "gif")
	   (opticl:read-gif-file file))
	  (t (error "unsupported file format ~s ~s" extension file)))))

(struct-to-clos:struct->class
 (defstruct (opticl-loaded-surface)
   data
   width
   height))

(defparameter *test* (sketch-util::opticl-load-image "/home/imac/Pictures/Blank+_5852f2f085f3a33a2a14eda1a11fb6f0.png"))

(defun convert (opticl-data)
  (let ((dimensions (array-dimensions opticl-data))
	(type (array-element-type opticl-data)))
    (when (or (not (eq 'unsigned-byte
		       (first type)))
	      (member type '(single-float double-float fixnum)))
      (error "type not supported"))
    (let ((channels (or (third dimensions)
			1))
	  (width (first dimensions))
	  (height (second dimensions)))
      (let ((new (make-array (* width height 4)
			     :element-type '(unsigned-byte 8))))
	;;FIXME::bits not correct? 
	;;32 -> 8 = ash n -24
	;;16 -> 8 = ash n -8
	;;8 -> 8 = identity n
	;;4 -> 8 = 15 -> 255 = * n 255/15 = * n 17
	;;2 -> 8 = 3 -> 255 = * n 85
	;;1 -> 8 = 1 -> 255 = * n 255
	(flet ((u32->8 (n)
		 (declare (optimize (speed 3) (safety 0)))
		 (declare (type (unsigned-byte 32) n))
		 (ash n -24))
	       (u16->8 (n)
		 (declare (optimize (speed 3) (safety 0)))
		 (declare (type (unsigned-byte 16) n))
		 (ash n -8))
	       (u8->8 (n)
		 n)
	       (u4->8 (n)
		 (declare (optimize (speed 3) (safety 0)))
		 (declare (type (unsigned-byte 4) n))
		 (* n 17))
	       (u2->8 (n)
		 (declare (optimize (speed 3) (safety 0)))
		 (declare (type (unsigned-byte 2) n))
		 (* n 85))
	       (u1->8 (n)
		 (declare (optimize (speed 3) (safety 0)))
		 (declare (type (unsigned-byte 1) n))
		 (* n 255)))
	  (let ((convert-fun (ecase (second type)
			       (32 #'u32->8)
			       (16 #'u16->8)
			       (8 #'u8->8)
			       (4 #'u4->8)
			       (2 #'u2->8)
			       (1 #'u1->8))))
	    (flet ((convert-value (value)
		     (funcall convert-fun value)))
	      (flet ((dump-pixels-1 (w h)
		       (let ((gray
			      (convert-value (aref opticl-data w h))))
			 ;;FIXME::include alpha channel or not?
			 (values
			  gray
			  gray
			  gray
			  255)))
		     (dump-pixels-2 (w h)
		       (let ((gray (convert-value (aref opticl-data w h 0)))
			     (alpha (convert-value (aref opticl-data w h 1))))
			 (values gray gray alpha)))
		     (dump-pixels-3 (w h)
		       (values
			(convert-value (aref opticl-data w h 0))
			(convert-value (aref opticl-data w h 1))
			(convert-value (aref opticl-data w h 2))
			255))
		     (dump-pixels-4 (w h)
		       (values
			(convert-value (aref opticl-data w h 0))
			(convert-value (aref opticl-data w h 1))
			(convert-value (aref opticl-data w h 2))
			(convert-value (aref opticl-data w h 3)))))
		(let ((dump-pixels-fun (ecase channels
					  (1 #'dump-pixels-1)
					  (2 #'dump-pixels-2)
					  (3 #'dump-pixels-3)
					  (4 #'dump-pixels-4))))
		  (dotimes (w width)
		    (dotimes (h height)
		      (multiple-value-bind (r b g a) (funcall dump-pixels-fun w h)
			(let ((base (+ w (* width h))))
			  (setf (aref new (+ base 0)) r
				(aref new (+ base 1)) g
				(aref new (+ base 2)) b
				(aref new (+ base 3)) a))))))))))))))

;;a -> a a a 1.0 or a a a a?
;;ra -> r r r a
;;rgb -> r g b 1.0
;;rgba -> r g b a
