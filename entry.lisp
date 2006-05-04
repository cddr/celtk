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

;----------------------------------------------------------------------------

(deftk entry (widget)
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
    :xscrollcommand (c-in nil)
    :textvariable (c? (^path))
    :md-value (c-in "")))

(defmethod md-awaken :after ((self entry)) ;; move this to a traces slot on widget
  (with-integrity (:client `(:trace ,self))
    (tk-format-now "trace add variable ~a write TraceOP" (^path))
    (setf (gethash '|write| (event-handlers self))
      (lambda (self event-type) ;; &rest args)
        (declare (ignorable event-type))
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

(deftk text-widget (widget)
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
    :xscrollcommand (c-in nil)
    :yscrollcommand (c-in nil)
    :modified (c-in nil)
    :bindings (c? (list (list '|<<Modified>>|
                          (lambda (self event &rest args)
                            (eko ("<<Modified>> !!TK value for text-widget" self event args)
                              (setf (^modified) t))))))))


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