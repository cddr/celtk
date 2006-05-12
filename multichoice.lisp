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

; --- scale ----------------------------------------------

(in-package :Celtk)

(deftk scale (commander widget)
  ()
  (:tk-spec scale
    -activestyle  -background -borderwidth -cursor
    (tkfont -font) -foreground
    -highlightbackground -highlightcolor -highlightthickness
    -relief -state
    -takefocus -troughcolor -width -xscrollcommand -yscrollcommand
    -orient -repeatdelay
    -repeatinterval
    -bigincrement -command -digits -from
    (-tk-label -label) (-tk-length -length) -resolution
    -showvalue -sliderlength -sliderrelief
    -tickinterval -to (-tk-variable nil))
  (:default-initargs
      :id (gentemp "SCL")
      :md-value (c-in nil)
    :tk-variable nil ;;(c? (^path))
    :xscrollcommand (c-in nil)
    :yscrollcommand (c-in nil)
    :on-command (lambda (self value)
                  (setf (^md-value) value))))

(defmethod make-tk-instance :after ((self scale))
  "Still necessary?"
  (when (^md-value)
    (tk-format `(:variable ,self) "~a set ~a"  (^path) (^md-value))))

; --- listbox --------------------------------------------------------------

(deftk listbox (widget)
  ()  
  (:tk-spec listbox
    -activestyle  -background -borderwidth -cursor
    -disabledforeground -exportselection (tkfont -font) -foreground
    -height -highlightbackground -highlightcolor -highlightthickness
    -listvariable -relief -selectmode -selectbackground
    -selectborderwidth -selectforeground -setgrid -state
    -takefocus -width -xscrollcommand -yscrollcommand)
  (:default-initargs
      :id (gentemp "LBX")
    :xscrollcommand (c-in nil)
    :yscrollcommand (c-in nil)
      :bindings (c? (assert (selector self))
                  (when (selector self) ;; if not? Figure out how listbox tracks own selection
                      (list (list '|<<ListboxSelect>>|
                              (lambda (self event &rest args)
                                (let ((selection (parse-integer (tk-eval "~a curselection" (^path)))))
                                  (trc "NEW listbox callback firing" self event selection)
                                  (setf (selection (selector self))
                                    (md-value (elt (^kids) selection)))))))))))

(defmodel listbox-item (tk-object)
  ((item-text :initarg :item-text :accessor item-text
     :initform (c? (format nil "~a" (^md-value))))))

(defmethod make-tk-instance ((self listbox-item))
  (trc nil "make-tk-instance listbox-item insert" self)
  (tk-format `(:post-make-tk ,self) "~A insert end ~s" (path .parent) (^item-text)))

(defobserver .kids ((self listbox))
  (when old-value
    (tk-format `(:destroy ,self) "~A delete ~a ~a"
      (^path)
      0 (1- (length old-value)))))

; --- spinbox ---------------------------------------------

(deftk spinbox (commander widget)
  ((initial-value :initform nil :initarg :initial-value :reader initial-value))
  (:tk-spec spinbox
    -activebackground -background -borderwidth -cursor
    -buttonbackground -buttoncursor -buttondownrelief -buttonuprelief
    -disabledforeground  -disabledbackground -exportselection
    (tkfont -font) (spin-format -format) -foreground -from
    -command -invalidcommand -increment
    -highlightbackground -highlightcolor -highlightthickness 
    -insertbackground -insertborderwidth -insertofftime -insertontime
    -insertwidth -jump -justify -orient
    -padx -pady -relief -repeatdelay
    -repeatinterval -selectbackground -selectborderwidth -selectforeground
    -readonlybackground -state -to
    -takefocus -text -textvariable
    -troughcolor -underline -xscrollcommand  
    -validate -validatecommand (tk-values -values) -width -wrap)
  (:default-initargs
      :md-value (c-in nil)
      :id (gentemp "SPN")
      :textVariable (c? (^path))
    :xscrollcommand (c-in nil)
    :command (c? (format nil "call-back ~(~a~) %s" (^path)))
    :on-command (c? (lambda (self text)
                      (eko ("variable mirror command fired !!!!!!!" text)
                        (setf (^md-value) text))))))

(defobserver .md-value ((self spinbox))
  (when new-value
    (tk-format `(:variable ,self) "set ~a ~a" (^path) (tk-send-value new-value))))

(defobserver initial-value ((self spinbox))
  (when new-value
    (trc "spinbox intializing from initvalue !!!!!!!!!!!!" self new-value)
    (setf (^md-value) new-value)))


