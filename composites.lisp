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

(defmodel window (composite-widget)
  ((wish :initarg :wish :accessor wish
     :initform (wish-stream *wish*)
     #+(or) (c? (do-execute "wish85 -name testwindow" 
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


;--- group geometry -----------------------------------------

(defmodel inline-mixin (composite-widget)
  ((padx :initarg :padx :accessor padx :initform 0)
   (pady :initarg :pady :accessor pady :initform 0)
   (packing-side :initarg :packing-side :accessor packing-side :initform 'left)
   (layout-anchor :initarg :layout-anchor :accessor layout-anchor :initform 'nw))
  (:default-initargs
      :kid-slots (lambda (self)
                   (declare (ignore self))
                   (list
                    (mk-kid-slot (packing :if-missing t)
                      nil))) ;; suppress default
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

;--- scroller (of canvas; need to generalize this) ----------

(defmodel scroller (grid-manager frame)
  ((canvas :initarg :canvas :accessor canvas :initform nil))
  (:default-initargs
      :id :cv-scroller
    :kids-packing nil
    :gridding '(:columns ("-weight {1}" "-weight {0}")
                 :rows ("-weight {1}" "-weight {0}"))
    :kids (c? (the-kids
               (^canvas)
               (mk-scrollbar :id :hscroll
                 :orient "horizontal"
                 :gridding "-row 1 -column 0 -sticky we"
                 :command (c? (format nil "~a xview" (path (kid1 .parent)))))
               (mk-scrollbar :id :vscroll
                 :orient "vertical"
                 :gridding "-row 0 -column 1 -sticky ns"
                 :command (c? (format nil "~a yview" (path (kid1 .parent)))))))))

(defmacro mk-scroller (&rest iargs)
  `(make-instance 'scroller
     :fm-parent self
     ,@iargs))

(defmethod initialize-instance :after ((self scroller) &key)
  ;
  ; Tk does not do late binding on widget refs, so the canvas cannot mention the scrollbars
  ; in x/y scrollcommands since the canvas gets made first
  ;
  (with-integrity (:client `(:post-make-tk ,self))
    (setf (xscrollcommand (kid1 self)) (format nil "~a set" (path (fm! :hscroll))))
    (setf (yscrollcommand (kid1 self)) (format nil "~a set" (path (fm! :vscroll))))))

