#|

 Celtic / widget.lisp : Foundation classes

  Copyright (c) 2004 by Kenneth William Tilton <ktilton@nyc.rr.com>

 A work derived from Peter Herth's LTk. As a derived work,
 usage is governed by LTk's "Lisp LGPL" licensing:

 You have the right to distribute and use this software as governed by 
 the terms of the Lisp Lesser GNU Public License (LLGPL):

    (http://opensource.franz.com/preamble.html)
 
 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 Lisp Lesser GNU Public License for more details.
 
|#

(in-package :Celtk)

;;; --- toplevel ---------------------------------------------

(deftk toplevel ()
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

(deftk panedwindow ()
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
      :layout nil))

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

(defmodel window (family)
  ((wish :initarg :wish :accessor wish
     :initform (wish-stream *wish*)
     #+(or) (c? (do-execute "wish84 -name testwindow" 
                     nil #+not (list (format nil "-name ~s" (title$ self))))))
   (ewish :initarg :ewish :accessor ewish :initform nil :cell nil) ;; vestigial?
   (title$ :initarg :title$ :accessor title$
     :initform (c? (string (class-name (class-of self)))))
   (dictionary :initarg :dictionary :initform (make-hash-table) :accessor dictionary)
   (callbacks :initarg :callbacks :accessor callbacks
     :initform (make-hash-table :test #'eq))
   (edit-style :initarg :edit-style :accessor edit-style :initform (c-in nil))))

(defmethod path ((self window)) ".")
(defmethod parent-path ((self window)) "")
(defmethod kids-layout ((self window)) nil)


;--- group geometry -----------------------------------------

(defmodel inline-mixin ()
  ((kids-layout :initarg :kids-layout :accessor kids-layout :initform nil)
   (padx :initarg :padx :accessor padx :initform 0)
   (pady :initarg :pady :accessor pady :initform 0)
   (layout-side :initarg :layout-side :accessor layout-side :initform 'left)
   (layout-anchor :initarg :layout-anchor :accessor layout-anchor :initform 'nw))
  (:default-initargs
      :kid-slots (lambda (self)
                   (declare (ignore self))
                   (list
                    (mk-kid-slot (layout :if-missing t)
                      nil))) ;; suppress default
    :kids-layout (c? (format nil "pack~{ ~a~} -side ~a -anchor ~a -padx ~a -pady ~a"
                       (mapcar 'path (^kids))
                       (down$ (^layout-side))
                       (down$ (^layout-anchor))
                       (^padx)(^pady)))))

(defobserver kids-layout ()
  (when new-value
    (tk-format `(:pack ,self kids-layout) new-value)))

(defmodel row-mixin (inline-mixin)
  ()
  (:default-initargs
    :layout-side 'left))

(defmodel stack-mixin (inline-mixin)
  ()
  (:default-initargs
    :layout-side 'top))


;--- f r a m e --------------------------------------------------

(deftk frame ()
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

(deftk labelframe ()
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
