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

;;; --- running a Celtk application (window class, actually) --------------------------------------

(eval-when (compile load eval)
  (export '(tk-scaling run-window test-window)))



(defun run-window (root-class)
  (declare (ignorable root-class))
  (setf *tkw* nil)
  (cells-reset 'tk-user-queue-handler)
  (tk-interp-init-ensure)

  (setf *tki* (Tcl_CreateInterp))
  (tk-app-init *tki*)
  (tk-togl-init *tki*)

  #+soon (tk-format-now "proc trc2 {cb n1 n2 op} {puts \"(:callback \\\"$cb\\\" :name1 $n1 :name2 \\\"$n2\\\" :op $op)\"}")
  (tk-format-now "set tk-events {}")
  (tk-format-now "proc call-back {w args} {
global tk-events
lappend tk-events [concat do-on-command $w $args]}")
  ;; deadly (takes down ACL) -> (tk-format-now "bind . <Escape> exit")
    
  (with-integrity ()
    (setf *tkw* (make-instance root-class)))

  (tk-format `(:fini) "wm deiconify .")
    
  ;; one or the other of...

  ;; hangs on win close now, but probably easy to fix, just needs to know when
  ;; to stop looping: -> (tcl-do-one-event-loop)

  (tcl-do-one-event-loop)
  )

;; Our own event loop ! - Use this if it is desirable to do something
;; else between events

(defun tcl-do-one-event-loop ()
  (loop with start-time = (get-internal-real-time)
        while (> 10 (floor (- (get-internal-real-time) start-time) internal-time-units-per-second))
        do
        (bif (events (prog1
                         (tk-eval-list "set tk-events")
                       (tk-eval "set tk-events {}")))
          (loop ;; with x = (trc "no events")
                for e in events
              do (setf start-time (get-internal-real-time))
            (tk-process-event e))
          (sleep .05)) ;;*event-loop-delay*))
        (loop until (zerop (Tcl_DoOneEvent 2)))))

(defun tk-process-event (event)
  (trc "event string:" event)
  (destructuring-bind (fn w-name &rest args)
      (read-from-string (conc$ "(" event ")"))
    (let ((id (symbol-name w-name)))
      (bif (w (gethash id (dictionary *tkw*)))
        (progn (trc "funcalling" fn w)
          (apply fn w args))
        (progn
          (loop for k being the hash-keys of (dictionary *tkw*)
              do (trc "known key" k (type-of k)))
          (break "bad id ~a in event ~a" id event))))))

(defmethod do-on-command :around (self &rest args)
  (trc "on command!!!" self)
  (bwhen (ocb (on-command self))
    (apply ocb self args)))

(defun test-window (root-class)
  "nails existing window as a convenience in iterative development"
  (declare (ignorable root-class))

  #+tki (when (and *tkw* (open-stream-p *tkw*))
    (format *tkw* "wm withdraw .~%")
    (force-output *tkw*)
    (format *tkw* "destroy .%")
    (force-output *tkw*)
    (setf *tkw* nil))

  (run-window root-class))