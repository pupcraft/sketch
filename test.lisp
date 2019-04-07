(defparameter *dir* (merge-pathnames "src/" (asdf:system-source-directory :sketch)))
(defparameter *files*
  '((:file "package")
    (:file "math")
    (:file "utils")
    (:file "environment")
    (:file "resources")
    (:file "color")
    (:file "channels")
    (:file "shaders")
    (:file "pen")
    (:file "image")
    (:file "font")
    (:file "geometry")
    (:file "drawing")
    (:file "shapes")
    (:file "transforms")
    (:file "sketch")
    (:file "figures")
    (:file "controllers")))
(defparameter *newline* (format nil "~%"))
(defun concatenate-files ()
  (apply 'concatenate 'string
	 (apply 'append
		(mapcar (lambda (x)
			  (let ((name (second x)))
			    (list
			     ;;*newline*
			     ;;";;;;"
			     *newline*
			     ";;----------------------------------------------------------------------"
			     *newline*
			     ";"
			     name
			     ;;";;;;"
			     *newline*
			     *newline*
			     (alexandria:read-file-into-string
			      (merge-pathnames (concatenate 'string name ".lisp")
					       *dir*))
			     *newline*
			     )))
			*files*))))

(defun dump (&optional (x (concatenate-files)))
  (with-open-file
      (stream (merge-pathnames "dump.lisp" *dir*) :direction :output :if-does-not-exist :create)
    (princ x stream)))
