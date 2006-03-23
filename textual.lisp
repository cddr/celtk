#|

 Celtic / textual.lisp

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