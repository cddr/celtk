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

;--- button ----------------------------------------------

(deftk button ()
  ()
  (:tk-spec button
    -activebackground  -activeforeground  -anchor
    -background -bitmap -borderwidth -cursor
    -disabledforeground    -font -foreground
    -highlightbackground -highlightcolor -highlightthickness -image
    -justify 
    -padx -pady -relief -repeatdelay
    -repeatinterval -takefocus -text -textvariable
    -underline -wraplength
    -command -compound -default -height -overrelief -state -width)
  (:default-initargs
      :id (gentemp "B")))

(defmacro mk-button-ex ((text command) &rest initargs)
  `(make-instance 'button
     :fm-parent *parent*
     :text ,text
     :command (c? (tk-callback self 'cmd 
                    (lambda () ,command)))
     ,@initargs))

; --- checkbutton ---------------------------------------------

(deftk radiocheck () 
  ()
  (:tk-spec radiocheck
    -activebackground  -activeforeground  -anchor
    -background -bitmap -borderwidth -compound -cursor
    -disabledforeground   -font -foreground
    -highlightbackground -highlightcolor -highlightthickness -image
    -justify -padx -pady -relief -takefocus -text -textvariable
    -underline -wraplength 
    -command -height -indicatoron -offrelief 
    -overrelief -selectcolor -selectimage -state -tristateimage 
    -tristatevalue (tk-variable -variable) -width))


(deftk checkbutton (radiocheck)
  ()
  (:tk-spec checkbutton
    -offvalue -onvalue)
  (:default-initargs
      :id (gentemp "CK")
    :md-value (c-in nil)
    :tk-variable (c? (^path))
    :command (c? (tk-callback self 'toggle
                   (lambda ()
                     (declare (ignore id args))
                     (setf (^md-value) (not (^md-value))))))))

(defobserver .md-value ((self checkbutton))
  (tk-format `(:variable ,self) "set ~(~a~) ~a" (path self) (if new-value 1 0)))

; --- radiobutton -------------------------------------

(deftk radiobutton (radiocheck)
  ()
  (:tk-spec radiobutton
    -value)
  (:default-initargs
      :id (gentemp "RB")
      :tk-variable (c? (path (upper self selector)))
      :command (c? (tk-callback self 'radio-set
                     (lambda ()
                       (setf (selection (upper self selector)) (value self)))))))

(defmacro mk-radiobutton-ex ((text value) &rest initargs)
  `(make-instance 'radiobutton
     :fm-parent *parent*
     :text ,text
     :value ,value
     ,@initargs))

; --- scale ----------------------------------------------

(deftk scale ()
  ()
  (:tk-spec scale
    -activestyle  -background -borderwidth -cursor
    -font -foreground
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
    :command (c? (tk-callbackval self 'scale-set
                   (lambda (&rest args)
                     (declare (ignore id))
                     (setf (^md-value) (car args)))))))

(defmethod make-tk-instance :after ((self scale))
  "Still necessary?"
  (when (^md-value)
    (tk-format `(:variable ,self) "~a set ~a"  (^path) (^md-value))))

; --- listbox --------------------------------------------------------------

(deftk listbox ()
  ()  
  (:tk-spec listbox
    -activestyle  -background -borderwidth -cursor
    -disabledforeground -exportselection -font -foreground
    -height -highlightbackground -highlightcolor -highlightthickness
    -listvariable -relief -selectmode -selectbackground
    -selectborderwidth -selectforeground -setgrid -state
    -takefocus -width -xscrollcommand -yscrollcommand)
  (:default-initargs
      :id (gentemp "LBX")
    :xscrollcommand (c-in nil)
    :yscrollcommand (c-in nil)
      :bindings (c? (when (selector self) ;; if not? Figure out how listbox tracks own selection
                      (list (list  "<<ListboxSelect>>"
                              (format nil "{callbackval ~~a [~a curselection]}" (^path))
                              (lambda (selection)
                                (setf (selection (selector self))
                                  (md-value (elt (^kids) selection))))))))))

(defmodel listbox-item (tk-object)
  ((item-text :initarg :item-text :accessor item-text
     :initform (c? (format nil "~a" (^md-value))))))

(defmethod make-tk-instance ((self listbox-item))
  (tk-format `(:post-make-tk ,self) "~A insert end ~s" (path .parent) (^item-text)))

(defobserver .kids ((self listbox))
  (when old-value
    (tk-format `(:destroy ,self) "~A delete ~a ~a"
      (^path)
      0 (1- (length old-value)))))

; --- spinbox ---------------------------------------------

(deftk spinbox ()
  ((initial-value :initform nil :initarg :initial-value :reader initial-value))
  (:tk-spec spinbox
    -activebackground -background -borderwidth -cursor
    -buttonbackground -buttoncursor -buttondownrelief -buttonuprelief
    -disabledforeground  -disabledbackground -exportselection
    -font (spin-format -format) -foreground -from
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
    :command (c? (tk-callbackstring-x self 'vmirror "%s"
                   ;;;(tk-callback self 'vcmd
                   (lambda (text)
                     (eko ("variable mirror command fired !!!!!!!" text)
                       (setf (^md-value) text)))))))

(defobserver .md-value ((self spinbox))
  (when new-value
    (tk-format `(:variable ,self) "set ~a ~a" (^path) (tk-send-value new-value))))

(defobserver initial-value ((self spinbox))
  (when new-value
    (with-integrity (:change)
      (trc "spinbox intializing from initvalue !!!!!!!!!!!!" self new-value)
      (setf (^md-value) new-value))))


; --- scroll bars ----------------------------------------


(deftk scrollbar ()
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
                   :font '(courier 9)
                   :state (c? (if (enabled .parent) 'normal 'disabled))
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
          (item-no (position new-value (^list-item-keys))))
      (tk-format `(:selection ,self) "~(~a~) selection set ~a" (path lb) item-no))))


