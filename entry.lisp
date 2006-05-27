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

;----------------------------------------------------------------------------

(deftk entry (widget)
  ((text :initarg :text :accessor text :initform nil))
  (:tk-spec entry
    -background -borderwidth -cursor
    -disabledforeground  -disabledbackground -exportselection (tkfont -font) -foreground
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
    :xscrollcommand (c-in nil)
    :textvariable (c? (intern (^path)))
    :event-handler (lambda (self xe)
                     (TRC nil "widget-event-handler" self (xsv type xe) )
                     (case (tk-event-type (xsv type xe))
                       (:virtualevent
                        (trc nil "v/e" (xsv name xe))
                        (case (read-from-string (string-upcase (xsv name xe)))
                          (trace
                           (TRC nil "entry e/h trace" self (when (plusp (xsv user-data xe))
                                                              (tcl-get-string (xsv user-data xe))))
                           ;; assuming write op, but data field shows that
                           (let ((new-value (tcl-get-var *tki* (^path)
                                              (var-flags :TCL_NAMESPACE_ONLY))))
                             (unless (string= new-value (^md-value))
                               (setf (^md-value) new-value))))))))
   
    :md-value (c-in "")))

(defmethod md-awaken :after ((self entry)) ;; move this to a traces slot on widget
  (with-integrity (:client `(:trace ,self))
    (tk-format-now "trace add variable ~a write TraceOP" (^path))))
 
;;; /// this next replicates the handling of tk-mirror-variable because
;;; those leverage the COMMAND mechanism, which entry lacks
;;
(defobserver .md-value ((self entry))
  (when new-value 
    (unless (string= new-value old-value)
      (trc nil "md-value output" self new-value)
      (tk-format `(:variable ,self) "set ~a ~s" (^path) new-value))))

(deftk text-widget (widget)
  ((modified :initarg :modified :accessor modified :initform nil)
   (eval-text :initarg :eval-text :accessor eval-text :initform (c-in t)
	      :documentation "Set to nil if you want to make sure text entries do not get evaluated. If set to nil the /dangerous charachters/ will be replaced by space char."))
  (:tk-spec text
    -background -borderwidth -cursor
    -exportselection (tkfont -font) -foreground
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
    :xscrollcommand (c-in nil)
    :yscrollcommand (c-in nil)
    :modified (c-in nil)
    :borderwidth (c? (if (^modified) 8 2))
    :event-handler (lambda (self xe)
                     (case (tk-event-type (xsv type xe))
                       (:virtualevent
                        (case (read-from-string (string-upcase (xsv name xe)))
                          (modified
                           (eko (nil "<<Modified>> !!TK value for text-widget" self)
                             (setf (^modified) t)))))
                       ))))

(defobserver .md-value ((self text-widget))
  (trc nil "md-value output" self new-value)
  (with-integrity (:client `(:variable ,self))
    (tk-format-now "~a delete 1.0 end" (^path))
    (let ((value nil))
      (when (plusp (length new-value))
	(if (not (^eval-text))
	  (setq value (replace-dangerous-chars new-value))
	  (setq value new-value))
	(tk-format-now "~a insert end ~s" (^path) value)))))

;; frgo, 2006-05-27:
;; replace-dangeorous-chars is meant to replace characters in a
;; sequence that would start/end evaluation in Tcl land.
(defun replace-dangerous-chars (seq &optional (dangerous-chars "[]{}"))
  (assert (stringp seq))
  (let ((result seq))
    (loop for pos from 0 to (1- (length result))
      do
      (let ((c (char result pos)))
        (if (find c dangerous-chars)
	 (setf (char result pos) #\Space))))
    (values result)))

;;;(defvar +tk-keysym-table+
;;;  (let ((ht (make-hash-table :test 'string=)))
;;;    (with-open-file (ksyms "/0dev/math-paper/tk-keysym.dat" :direction :input)
;;;      (loop for ksym-def = (read-line ksyms nil nil)
;;;          for end = (position #\space ksym-def)
;;;          while end
;;;          do (let ((ksym (subseq ksym-def 0 end)))
;;;               (setf (gethash ksym ht) (read-from-string ksym-def nil nil :start (1+ end))))
;;;          finally (return ht)))))

 (defun tk-translate-keysym (keysym$)
   (if (= 1 (length keysym$))
      (schar keysym$ 0)
    (intern (string-upcase keysym$))
    #+nah (gethash keysym$ +tk-keysym-table+)))