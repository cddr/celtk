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

(in-package :celtk)

;--- group geometry -----------------------------------------

(defmd inline-mixin (composite-widget widget)
  (padx 0)
  (pady 0)
  (packing-side 'left)
  (layout-anchor 'nw)
  :kids-packing (c? (when (^kids)
		      (format nil "pack~{ ~a~} -side ~a -anchor ~a -padx ~a -pady ~a"
			      (mapcar 'path (^kids))
			      (down$ (^packing-side))
			      (down$ (^layout-anchor))
			      (^padx)(^pady)))))

(defobserver kids-packing ()
  (when new-value
    (tk-format `(:pack ,self kids-packing) new-value)))

(defmd row-mixin (inline-mixin)
  :packing-side 'left)

(defmd stack-mixin (inline-mixin)
  :packing-side 'top)

;--- g r i d s ----------------------------------------------

(defmd grid (grid-manager frame)
  (rows)
  (row-factory)
  (kids (c? (the-kids
	     (loop for row in (^rows)
		for row-num from 0
		collect (funcall (^row-factory)
				 row
				 row-num))))))

(defobserver .kids ((self grid))
  (when new-value
    (loop for k in new-value
	 when (gridding k)
	 do (tk-format `(:grid ,k)
		       (format nil "grid ~a ~a"
			       (path k)
			       (gridding k))))))

(defmacro mk-grid (&rest initargs)
  `(make-instance 'grid
    ,@initargs
    :fm-parent *parent*))


;--- f r a m e --------------------------------------------------

(deftk frame (composite-widget widget)
  ()
  (:tk-spec frame -borderwidth -cursor	-highlightbackground -highlightcolor
    -highlightthickness -padx -pady -relief
    -takefocus -background (tk-class -class) 
    -colormap -container -height -visual -width)
  (:default-initargs
      :id (gentemp "F")))

(deftk frame-selector (tk-selector frame) ())
(deftk frame-row (row-mixin frame-selector)())
(deftk frame-stack (stack-mixin frame-selector)())


;--- l a b e l f r a m e ----------------------------------------------

(deftk labelframe (widget)
  ()
  (:tk-spec labelframe -borderwidth -cursor -highlightbackground -highlightcolor
    -highlightthickness -padx -pady -relief
    -takefocus -background (tk-class -class) -colormap -container -height -visual -width
    -text -labelanchor -labelwidget)
  (:default-initargs
      :id (gentemp "LF")))

(deftk labelframe-selector (tk-selector labelframe)())
(deftk labelframe-row (row-mixin labelframe-selector)())
(deftk labelframe-stack (stack-mixin labelframe-selector)())

;;; --- handy macros

(defmacro def-mk-inline (name (unlabelled labelled))
  `(defmacro ,name ((&rest initargs) &rest kids)
     (if (evenp (length initargs))
         `(make-instance ',',unlabelled
            :fm-parent *parent*
            ,@initargs
            :kids (c? (the-kids ,@kids)))
       `(make-instance ',',labelled
          :fm-parent *parent*
          :text ,(car initargs)
          ,@(cdr initargs)
          :kids (c? (the-kids ,@kids))))))

(def-mk-inline mk-row (frame-row labelframe-row))
(def-mk-inline mk-stack (frame-stack labelframe-stack))

