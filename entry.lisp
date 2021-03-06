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

;;; $Header$

(in-package :celtk)

;----------------------------------------------------------------------------

(deftk entry (widget)
  ((text :initarg :text :accessor text :initform nil))
  (:tk-spec entry
    -background -borderwidth -cursor
    -disabledforeground  -disabledbackground -exportselection (tkfont -font) -foreground
    -highlightbackground -highlightcolor -highlightthickness
    -insertbackground -insertborderwidth -insertofftime -insertontime
    -insertwidth  (tk-justify -justify) 
    -relief -selectbackground -selectborderwidth -selectforeground
    -takefocus -textvariable
    -xscrollcommand
    -invalidcommand -readonlybackground -show -state 
    -validate -validatecommand -width )
  (:default-initargs
      :id (gentemp "ENT")
    :textvariable (c? (intern (^path)))
    :xscrollcommand (c-in nil)
    :event-handler (lambda (self xe)
                     (TRC "ENTRY event-handler" self (xsv type xe) (tk-event-type (xsv type xe)))
                     (case (tk-event-type (xsv type xe))
                       (:virtualevent
                        (trc "ENTRY virtual event" (xsv name xe))
                        (case (intern (string-upcase (xsv name xe))
				      :keyword)
                          (:trace
			   (trc "trace: " self (when (pointerp (xsv user-data xe))
						 (tcl-get-string (xsv user-data xe))))
                           ;; assuming write op, but data field shows that
                           (let ((new-value (tcl-get-var *tki* (^path)
							 (var-flags :TCL-NAMESPACE-ONLY))))
                             (unless (string= new-value (^value))
                               (setf (^value) new-value))))))))
    :value (c-in "")))

(defmethod md-awaken :after ((self entry)) ;; move this to a traces slot on widget
  (with-integrity (:client `(:trace ,self))
    (tk-format-now "trace add variable ~a write TraceOP" (^path))))
 

(deftk text-widget (widget)
  ((modified :initarg :modified :accessor modified :initform nil))
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
      :value (c-in "<your text here>")
    :tile? nil
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

(defmethod clear ((self text-widget))
  (setf (value self) nil))

(defobserver .value ((self text-widget))
  (trc nil "value output" self new-value)
  (with-integrity (:client `(:variable ,self))
    (tk-format-now "~a delete 1.0 end" (^path))
    (when (plusp (length new-value))
      (tk-format-now "~a insert end {~a}" (^path) new-value)) ;; kt060528: simple {} seems to block evaluation
    ;; Yes, it does. But we had to change ~s to ~a also in order to prevent
    ;; side effects - frgo 2006-05-29 1:30 am ;-)
    (tk-format-now "update idletasks"))) ;; Causes a display update after each text widget operation.

(defobserver .value ((self entry))
  (trc nil "value output" self new-value)
  (with-integrity (:client `(:variable ,self))
    (tk-format-now "~a delete 0 end" (^path))
    (when (plusp (length new-value))
      (tk-format-now "~a insert end {~a}"
		     (^path)
		     new-value))
    (tk-format-now "update idletasks")))
