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


; --- scroll bars ----------------------------------------


(deftk scrollbar (widget)
  ()
  (:tk-spec scrollbar
    -activebackground -activerelief
    -background -borderwidth -command -cursor
    -elementborderwidth
    -highlightbackground -highlightcolor -highlightthickness
    -jump -orient -relief -repeatdelay
    -repeatinterval  -takefocus
    -troughcolor -width)
  (:default-initargs
      :id (gentemp "SBAR")))

(deftk scrolled-list (row-mixin frame-selector)
  ((list-item-keys :initarg :list-item-keys :accessor list-item-keys :initform nil)
   (list-item-factory :initarg :list-item-factory :accessor list-item-factory :initform nil)
   (list-height :initarg :list-height :accessor list-height :initform nil))
  (:default-initargs
      :list-height (c? (max 1 (length (^list-item-keys))))
    :kids-packing nil
      :kids (c? (the-kids
                 (mk-listbox :id :list-me
                   :kids (c? (the-kids
                              (mapcar (list-item-factory .parent)
                                (list-item-keys .parent))))
                   :tkfont '(courier 9)
                   :state (c? (if (enabled .parent) 'normal 'disabled))
                   :takefocus (c? (if (enabled .parent) 1 0))
                   :height (c? (list-height .parent))
                   :packing (c? (format nil "pack ~a -side left -fill both -expand 1" (^path)))
                   :yscrollcommand (c? (when (enabled .parent)
                                         (format nil "~a set" (path (nsib))))))
                 (mk-scrollbar :id :vscroll
                     :packing (c?pack-self "-side right -fill y")
                     :command (c? (format nil "~a yview" (path (psib)))))))))

(defmethod tk-output-selection :after ((self scrolled-list) new-value old-value old-value-boundp)
  (declare (ignorable old-value old-value-boundp))
  (trc nil "scrolled-list selection output" self new-value)
  (when new-value
    (let ((lb (car (^kids)))
          (item-no (position new-value (^list-item-keys) :test 'equal)))
      (if item-no
          (tk-format `(:selection ,self) "~(~a~) selection set ~a" (path lb) item-no)
        (break "~&scrolled-list ~a selection ~a not found in item keys ~a" self new-value (^list-item-keys))))))


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

