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

(defun bind (self event-type handler &optional (desired-event-info "")
              &aux (binding-key (intern (symbol-name event-type)))) ;; lookup on rebound will have been read in this package
  (trc "bind registering" self binding-key)
  (setf (gethash binding-key (event-handlers self)) handler)
  (tk-format `(:bind ,self) "bind ~a ~a {call-back-event %W ~:*\"~a\" ~a}"
    (^path) binding-key (or desired-event-info "")))

(defun run-window (root-class)
  (declare (ignorable root-class))
  (setf *tkw* nil)
  (cells-reset 'tk-user-queue-handler)
  (tk-interp-init-ensure)

  (setf *tki* (Tcl_CreateInterp))
  (tk-app-init *tki*)
  (tk-togl-init *tki*)

  (tk-format-now "proc TraceOP {n1 n2 op} {call-back-event $n1 $op}")
  (tk-format-now "set tk-events {}")
  (tk-format-now "proc call-back {w args} {global tk-events; lappend tk-events [concat do-on-command \\\"$w\\\" $args]}")
  (tk-format-now "proc call-back-event {w e args} {global tk-events; lappend tk-events [concat do-on-event \\\"$w\\\" \\\"$e\\\" $args]}")
  ;; (tk-format-now "bind . <Escape> {call-back-event %W :type <Escape> :time %t}")
    
  (with-integrity ()
    (setf *tkw* (make-instance root-class))
    (bind *tkw* '|<Escape>|
      (lambda (self &rest args)
        (trc "better event handler!!!!" self args))
      ":time %t"))

  (tk-format `(:fini) "wm deiconify .")
    
  ;; one or the other of...
  (tcl-do-one-event-loop)
  #+either-or (Tk_MainLoop)
  )

;; Our own event loop ! - Use this if it is desirable to do something
;; else between events

(defparameter *event-loop-delay* 0.08 "Minimum delay [s] in event loop not to lock out IDE (ACL anyway)")

(defun tcl-do-one-event-loop ()
  (loop with start-time = (get-internal-real-time)
        while (and (plusp (tk-get-num-main-windows))
                (> 10 (floor (- (get-internal-real-time) start-time) internal-time-units-per-second)))
        do
        (bif (events (prog1
                         (tk-eval-list "set tk-events")
                       (tk-eval "set tk-events {}")))
          (progn
            #+shhh (loop for e in events
              do (trc "event preview" e))
            (trc "main windows count =" (tk-get-num-main-windows))
            (loop for e in events
              do (setf start-time (get-internal-real-time))
            (tk-process-event e)))
          (sleep *event-loop-delay*))
        (loop until (zerop (Tcl_DoOneEvent 2)))
        finally (trc "tcl-do-one-event-loop has left the building")))

(defun tk-process-event (event)
  (destructuring-bind (fn w-name &rest args)
      (read-from-string (conc$ "(" event ")"))
    (let (#+nahh (id (symbol-name w-name)))
      (bif (w (gethash w-name (dictionary *tkw*)))
        (progn (trc nil "funcalling" fn w)
          (apply fn w args))
        (progn
          (loop for k being the hash-keys of (dictionary *tkw*)
              do (trc "known key" k (type-of k)))
          (break "bad id ~a in event ~a" w-name event))))))

(defmethod do-on-event (self event-type$ &rest args &aux (event-type (intern event-type$)))
  (assert (symbolp event-type))
  (trc "on event!!!" self event-type args)
  (bif (ecb (gethash event-type (event-handlers self)))
    (apply ecb self event-type args)
    (progn
      (trc "no event handlers for" self event-type)
      (loop for k being the hash-keys of (event-handlers self)
              do (trc "known key" k (type-of k))))))

(defmethod do-on-command (self &rest args)
  (bif (ocb (on-command self))
    (apply ocb self args)
    (trc "weird, no on-command value" self args)))

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