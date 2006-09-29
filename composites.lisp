;; -*- mode: Lisp; Syntax: Common-Lisp; Package: cells; -*-
#|

    Celtk -- Cells, Tcl, and Tk

Copyright (C) 2006 by Kenneth Tilton

This library is free software; you can redistribute it and/or
modify it under the terms of the Lisp Lesser GNU Public License
 (http://opensource.franz.com/preamble.html), known as the LLGPL.

This library is distributed  WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

See the Lisp Lesser GNU Public License for more details.

|#

(in-package :Celtk)

(eval-now!
  (export '(title$ active .time decoration)))

(export! application
         keyboard-modifiers
	 iconify
	 deiconify
	 full-screen-no-deco-window
	 screen-width
	 screen-height)

;;; --- decoration -------------------------------------------

(defmd decoration-mixin ()
  (decoration (c-in :normal)))

;;; --- toplevel ---------------------------------------------

(deftk toplevel (widget decoration-mixin)
  ()
  (:tk-spec toplevel
    -borderwidth -cursor -highlightbackground -highlightcolor
    -highlightthickness -padx -pady -relief
    -takefocus -background -tk-class -colormap
    -container -height -menu -screen
    -use -visual -width)
  (:default-initargs
      :id (gentemp "TOP")))

;; --- panedwindow -----------------------------------------

(deftk panedwindow (widget decoration-mixin)
  ()
  (:tk-spec panedwindow
    -background -borderwidth -cursor -height
    -orient -relief -width
    -handlepad
    -handlesize
    -opaqueresize
    -sashcursor
    -sashpad
    -sashrelief
    -sashwidth
    -showhandle)
  (:default-initargs
      :id (gentemp "PW")
      :packing nil))

(defmethod make-tk-instance ((self panedwindow))
  (tk-format `(:make-tk ,self) "panedwindow ~a -orient ~(~a~)"
    (^path) (or (orient self) "vertical"))
  (tk-format `(:pack ,self) "pack ~a -expand yes -fill both" (^path)))

(defmethod parent-path ((self panedwindow)) (^path))

(defobserver .kids ((self panedwindow))
  (loop for k in (^kids)
      do (trc "panedwindow adds" k (type-of k) (md-name k) (path k))
        (tk-format `(:post-make-tk ,self) "~a add ~a" (^path) (path k))))

; --------------------------------------------------------

(defmodel composite-widget (widget)
  ((kids-packing :initarg :kids-packing :accessor kids-packing :initform nil)))

(defvar *app*)

(defmodel application (family)
  ((app-time :initform (c-in (get-internal-real-time))
     :initarg :app-time
     :accessor app-time)))

(define-symbol-macro .time (app-time *app*))

(defmethod path ((self application)) nil)

(defun app-idle (self)
  (setf (^app-time) (get-internal-real-time)))

(defmd window (composite-widget decoration-mixin)
  (title$ (c? (string-capitalize (class-name (class-of self)))))
  (dictionary (make-hash-table :test 'equalp))
  (tkwins (make-hash-table))
  (xwins (make-hash-table))
  (keyboard-modifiers (c-in nil))
  (callbacks (make-hash-table :test #'eq))
  (edit-style (c-in nil))
  (tk-scaling (c? 1.3 #+tki (read-from-string (tk-eval "tk scaling"))))
  tkfonts-to-load
  tkfont-sizes-to-load
  (tkfont-info (tkfont-info-loader))
  initial-focus
  on-key-down
  on-key-up)

(defun screen-width ()
  (let ((*tkw* *tkw*))
    (tk-format-now "winfo screenwidth .")))

(defun screen-height ()
  (let ((*tkw* *tkw*))
    (tk-format-now "winfo screenheight .")))

(defmodel full-screen-no-deco-window (window)
  ())

(defmethod initialize-instance :before ((self full-screen-no-deco-window)
					&key &allow-other-keys)
  (tk-format '(:pre-make-tk self)
	     "wm geometry . [winfo screenwidth .]x[winfo screenheight .]+0+0")
  (tk-format '(:pre-make-tk self) "update idletasks")
  #-macosx (tk-format '(:pre-make-tk self) "wm attributes . -topmost yes")
  (tk-format '(:pre-make-tk self) "wm overrideredirect . yes")
  )

(defmethod do-on-key-down :before (self &rest args &aux (keysym (car args)))
  (trc nil "ctk::do-on-key-down window" keysym (keyboard-modifiers .tkw))
  (bwhen (mod (keysym-to-modifier keysym))
    (eko (nil "modifiers after adding" mod)
      (pushnew mod (keyboard-modifiers .tkw)))))

(defmethod do-on-key-up :before (self &rest args &aux (keysym (car args)))
  (trc nil "ctk::do-on-key-up before" keysym (keyboard-modifiers .tkw))
  (bwhen (mod (keysym-to-modifier keysym))
    (eko (nil "modifiers after removing" mod)
      (setf (keyboard-modifiers .tkw)
        (delete mod (keyboard-modifiers .tkw))))))

;;; Helper function that actually executes decoration change
(defun %%do-decoration (widget decoration)
  (let ((path (path widget)))
    (ecase decoration
      (:none    (progn
		  (tk-format '(:pre-make-tk decoration)
			     "wm withdraw ~a" path)    
		  (tk-format '(:pre-make-tk decoration)
			     "wm overrideredirect ~a 1" path)
		  (tk-format '(:pre-make-tk decoration)
			     "wm deiconify ~a" path)
		  (tk-format '(:pre-make-tk decoration)
			     "update idletasks" path)
		  ))
      (:normal  (progn
		  (tk-format '(:pre-make-tk decoration)
			     "wm withdraw ~a" path)    
		  (tk-format '(:pre-make-tk decoration)
			     "wm overrideredirect ~a 0" path)
		  (tk-format '(:pre-make-tk decoration)
			     "wm deiconify ~a" path)
		  (tk-format '(:pre-make-tk decoration)
			     "update idletasks" path))))))

;;; Decoration observer for all widgets that inherit from decoration-mixin
;;; On Mac OS X this is a one-way operation. When created without decorations
;;; then it is not possible to restore the decorations and vice versa. So on
;;; OS X the window decoration will stay as you created the window with.

(defobserver decoration ((self decoration-mixin)) ;; == wm overrideredirect 0|1
  (assert (or (eq new-value nil)        ;; Does not change decoration
	      (eq new-value :normal)    ;; "normal"
              (eq new-value :none)))    ;; No title bar, no nothing ...
  (if (not (eq new-value old-value))
      (%%do-decoration self new-value)))

(defobserver initial-focus ()
  (when new-value
    (tk-format '(:fini new-value) "focus ~a" (path new-value))))

(defun tkfont-info-loader ()
  (c? (eko (nil "tkfinfo")
        (loop with scaling = (^tk-scaling)
            for (tkfont fname) in (^tkfonts-to-load)
            collect (cons tkfont
                      (apply 'vector
                        (loop for fsize in (^tkfont-sizes-to-load)
                            for id = (format nil "~(~a-~2,'0d~)" tkfont fsize)
                            for tkf = (tk-eval "font create ~a -family {~a} -size ~a"
                                        id fname fsize)
                            for (nil ascent nil descent nil linespace nil fixed) = (tk-eval-list "font metrics ~a" tkf)
                            collect 
                              (progn (trc nil "tkfontloaded" id fname fsize tkfont tkf)
                                (make-tkfinfo :ascent (round (parse-integer ascent) scaling)
                                  :id id
                                  :family fname
                                  :size fsize
                                  :descent (round (parse-integer descent) scaling)
                                  :linespace (round (parse-integer linespace) scaling)
                                  :fixed (plusp (parse-integer fixed))
                                  :em (round (parse-integer
                                              (tk-eval "font measure ~(~a~) \"m\"" tkfont))
                                        scaling))))))))))

(defobserver title$ ((self window))
   (tk-format '(:configure "title") "wm title . ~s" (or new-value "Untitled")))

(defmethod path ((self window)) ".")
(defmethod parent-path ((self window)) "")

(defmethod iconify ((self window))
  (%%do-decoration self :normal)
  (tk-format `(:fini) "wm iconify ~a" (^path)))

(defmethod deiconify ((self window))
  (%%do-decoration self (decoration self))
  (tk-format `(:fini) "wm deiconify ~a" (^path)))




