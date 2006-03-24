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

(deftk label ()
  ()
  (:tk-spec label
    -activebackground   -activeforeground  -anchor
    -background -bitmap -borderwidth -compound -cursor
    -disabledforeground   -font -foreground
    -highlightbackground -highlightcolor -highlightthickness -image
    -justify
    -padx -pady -relief -takefocus -text -textvariable -underline
    -height -state -width -wraplength)
  (:default-initargs
      :id (gentemp "LBL")))

;--------------------------------------------------------------------------

(deftk message ()
  ()
  (:tk-spec message
    -activebackground   -activeforeground  -anchor
    -background -bitmap -borderwidth -compound -cursor
    -disabledforeground    -font -foreground
    -highlightbackground -highlightcolor -highlightthickness -image
    -justify 
    -padx -pady -relief 
    -takefocus -text -textvariable
    -underline -wraplength -width -state -height)
  (:default-initargs
      :id (gentemp "MSG")))

;----------------------------------------------------------------------------

(deftk entry ()
  ((text :initarg :text :accessor text :initform nil))
  (:tk-spec entry
    -background -borderwidth -cursor
    -disabledforeground  -disabledbackground -exportselection -font -foreground
    -highlightbackground -highlightcolor -highlightthickness
    -insertbackground -insertborderwidth -insertofftime -insertontime
    -insertwidth  -justify 
    -relief -selectbackground -selectborderwidth -selectforeground
    -takefocus -textvariable
    -xscrollcommand
    -invalidcommand -readonlybackground -show -state 
    -validate -validatecommand -width )
  (:default-initargs
      :id (gentemp "ENT")
    :textvariable (c? (^path))
      :md-value (c-in "")))

(defmethod md-awaken :after ((self entry))
  (tk-format `(:trace ,self) "trace add variable ~a write \"trc2 ~a\""
    (^path)
    (register-callback self 'tracewrite
      (lambda (&key name1 name2 op)
        (declare (ignorable name1 name2 op))
        (trc nil "tracewrite BINGO!!!!" (^path) (tk-eval-var (^path)))
        (let ((new-value (tk-eval-var (^path))))
          (unless (string= new-value (^md-value))
            (setf (^md-value) new-value)))))))
 
;;; /// this next replicates the handling of tk-mirror-variable because
;;; those leverage the COMMAND mechanism, which entry lacks
;;
(defobserver .md-value ((self entry))
  (when new-value 
    (unless (string= new-value old-value)
      (trc nil "md-value output" self new-value)
      (tk-format `(:variable ,self) "set ~a ~s" (^path) new-value))))

(deftk text-widget ()
  ((modified :initarg :modified :accessor modified :initform nil))
  (:tk-spec text
    -background -borderwidth -cursor
    -exportselection -font -foreground
    -highlightbackground -highlightcolor -highlightthickness 
    -insertbackground -insertborderwidth -insertofftime -insertontime
    -insertwidth -padx -pady -relief 
    -selectbackground -selectborderwidth -selectforeground
    -setgrid -takefocus -xscrollcommand -yscrollcommand
    -autoseparators -blockcursor -endline -height
    -inactiveselectionbackground -maxundo
    -spacing1 -spacing2 -spacing3 -startline 
    -state -tabs -tabstyle
    -undo -width -wrap)
  (:default-initargs
      :id (gentemp "TXT")
      :md-value (c-in "<your text here>")
    :modified (c-in nil)
    :bindings (c? (list (list "<<Modified>>"
                          (format nil "{callback ~~a}" (^path))
                          (lambda () ;;(self key &rest args)
                            (eko ("<<Modified>> !!!!!!!!!!!!!!!!!!TK value for text-widget" self)
                              (setf (^modified) t))))))))