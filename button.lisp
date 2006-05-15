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

(defcallback foo :int ((a :int) (b :int))
   (declare (ignore b))
   a)

;--- button ----------------------------------------------

(deftk button (commander widget)
  ()
  (:tk-spec button
    -activebackground  -activeforeground  -anchor
    -background -bitmap -borderwidth -cursor
    -disabledforeground    (tkfont -font) -foreground
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
     :on-command (c? (lambda (self)
                       (declare (ignorable self))
                       ,command))
     ,@initargs))

; --- checkbutton ---------------------------------------------

(deftk radiocheck (commander widget) 
  ()
  (:tk-spec radiocheck
    -activebackground  -activeforeground  -anchor
    -background -bitmap -borderwidth -compound -cursor
    -disabledforeground   (tkfont -font) -foreground
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
    :on-command (lambda (self)
                  (setf (^md-value) (not (^md-value))))))

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
      :on-command (lambda (self)
                 (setf (selection (upper self selector)) (value self)))))

(defmacro mk-radiobutton-ex ((text value) &rest initargs)
  `(make-instance 'radiobutton
     :fm-parent *parent*
     :text ,text
     :value ,value
     ,@initargs))