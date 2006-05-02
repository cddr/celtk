;; -*- mode: Lisp; Syntax: Common-Lisp; Package: celtk; -*-
;;;
;;; Copyright (c) 2006 by Kenneth William Tilton.
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy 
;;; of this software and associated documentation files (the "Software"), to deal 
;;; in the Software without restriction, including without limitation the rights 
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell 
;;; copies of the Software, and to permit persons to whom the Software is furnished 
;;; to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in 
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS 
;;; IN THE SOFTWARE.


(in-package :Celtk)

;--- group geometry -----------------------------------------

(defmodel inline-mixin (composite-widget)
  ((padx :initarg :padx :accessor padx :initform 0)
   (pady :initarg :pady :accessor pady :initform 0)
   (packing-side :initarg :packing-side :accessor packing-side :initform 'left)
   (layout-anchor :initarg :layout-anchor :accessor layout-anchor :initform 'nw))
  (:default-initargs
      :kid-slots (lambda (self) ;; /// vestigial? packing now defaults to nil anyway, methinks
                   (declare (ignore self))
                   (list
                    (mk-kid-slot (packing :if-missing t)
                      nil))) ;; suppress default (but see overall comment a few lines up)
    :kids-packing (c? (when (^kids)
                        (format nil "pack~{ ~a~} -side ~a -anchor ~a -padx ~a -pady ~a"
                          (mapcar 'path (^kids))
                          (down$ (^packing-side))
                          (down$ (^layout-anchor))
                          (^padx)(^pady))))))

(defobserver kids-packing ()
  (when new-value
    (tk-format `(:pack ,self kids-packing) new-value)))

(defmodel row-mixin (inline-mixin)
  ()
  (:default-initargs
    :packing-side 'left))

(defmodel stack-mixin (inline-mixin)
  ()
  (:default-initargs
    :packing-side 'top))


;--- f r a m e --------------------------------------------------

(deftk frame (composite-widget)
  ()
  (:tk-spec frame -borderwidth -cursor	-highlightbackground -highlightcolor
    -highlightthickness -padx -pady -relief
    -takefocus -background (tk-class -class) 
    -colormap -container -height -visual -width)
  (:default-initargs
      :id (gentemp "F")))

(deftk frame-selector (selector frame) ())
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

(deftk labelframe-selector (selector labelframe)())
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

