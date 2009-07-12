;; -*- mode: Lisp; Syntax: Common-Lisp; Package: cells; -*-
#|

    Celtk -- Cells, Tcl, and Tk

Copyright (C) 2009 by Andy Chambers

This library is free software; you can redistribute it and/or
modify it under the terms of the Lisp Lesser GNU Public License
 (http://opensource.franz.com/preamble.html), known as the LLGPL.

This library is distributed  WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

See the Lisp Lesser GNU Public License for more details.

|#

(in-package :celtk)

;;; Creates a pathname with NAME and TYPE in the same
;;; directory/host/device/whatever as this lisp file. Tries to get
;;; that at compile time to cope with some useful ASDF extensions that
;;; place fasls in arbitrary places.
(defun data-pathname (name type)
  (merge-pathnames (make-pathname :name name :type type)
                   #.(or *compile-file-truename* *load-truename*)))

(defmodel expander (frame-stack)
  ((label :initarg :label :accessor label :initform (c-in nil))
   (arrows :initarg :arrows :accessor arrows :initform (c-in nil))
   (expansion :initarg :expansion :accessor expansion :initform nil)
   (expanded :initarg :expanded :accessor expanded :initform (c-in nil)))
  (:default-initargs
      ;; if this doesn't work, try replacing the data-pathname
      ;; form with absolute file locations
      :arrows (list (list 'right (data-pathname "right" "gif"))
		    (list 'down (data-pathname "down" "gif")))
    :expanded (c-in t)))

(defobserver arrows ()
  (loop for (name filename) in (set-difference new-value old-value :key 'car)
     do (tk-format `(:pre-make-tk ,self)
		   "image create photo ~(~a.~a~) -file {~a}"
		   (^path) name (namestring filename))))

(defmacro mk-expander ((&rest inits) &body body)
  `(make-instance 'expander
		  ,@inits
		  :fm-parent *parent*
		  :expansion (c? (the-kids ,@body))
		  :kids-packing (c? (when (^kids)
				      (if (^expanded)
					  (format nil "pack~{ ~a~} -side top -anchor nw  -padx ~a -pady ~a -fill x"
						  (mapcar 'path (^kids))
						  (^padx) (^pady))
					  (with-output-to-string (s)
					    (format s "pack forget~{ ~a~}~%"
						    (mapcar 'path (cdr (^kids))))
					    (format s "pack ~a -side top -anchor nw -padx ~a -pady ~a -fill x"
						    (path (car (^kids)))
						    (^padx) (^pady))
					    s))))
		  :kids (c? (the-kids
			     (mk-button-ex ((^label)
					    (setf (expanded (upper self expander))
						  (not (expanded (upper self expander)))))
					   :image (c? (format nil "~(~a.~a~)" 
							      (path (upper self expander))
							      (if (expanded (upper self expander))
								  'down
								  'right)))
					   ;; can't use anchor/compound option on ttk button
					   :tile? nil 
					   :anchor 'w
					   :compound 'left)
			     (^expansion)))))


(defmodel expander-test (window)
  ()
  (:default-initargs
      :kids (c? (the-kids
		 (mk-stack (:packing (c?pack-self))
		   (mk-expander (:label "hi"
                                 :expanded (c-in nil)
                                 :width 100)
		     (mk-label :text "hisldkfj slkdjf sldkf jsldkfjdslkfj")
		     (mk-label :text "ho")))))))



;; 		   (mk-expander (:text "hi"
;; 				 :expanded t)


(defun test-expander ()
  (test-window 'expander-test))
