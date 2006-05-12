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

(defun bind (self event-type handler &optional (desired-event-info "")) ;; lookup on rebound will have been read in this package
  (trc "bind registering" self event-type)
  (setf (gethash event-type (event-handlers self)) handler)
  (tk-format `(:bind ,self) "bind ~a ~a {call-back-event %W ~:*\"~a\" ~a}"
    (^path) event-type (or desired-event-info "")))

(defun run-window (root-class)
  (declare (ignorable root-class))
  (setf *tkw* nil)
  (cells-reset 'tk-user-queue-handler)
  (tk-interp-init-ensure)

  (setf *tki* (Tcl_CreateInterp))
  ;; not recommended by Tcl doc (tcl-do-when-idle (get-callback 'tcl-idle-proc) 42)
  (tk-app-init *tki*)
  (tk-togl-init *tki*)

  (tk-format-now "proc TraceOP {n1 n2 op} {call-back-event $n1 $op}")
  (tk-format-now "set tk-events {}")
  (tk-format-now "proc call-back {w args} {global tk-events; lappend tk-events [concat do-on-command \\\"$w\\\" $args]}")
  (tk-format-now "proc call-back-event {w e args} {global tk-events; lappend tk-events [concat do-on-event \\\"$w\\\" \\\"$e\\\" $args]}")
  ;; (tk-format-now "bind . <Escape> {call-back-event %W :type <Escape> :time %t}")
    
  (with-integrity ()
    (setf *tkw* (make-instance root-class)))

  (tk-format `(:fini) "wm deiconify .")
  (tk-format-now "bind . <Escape> {destroy .}")

  ;; one or the other of...
  (tcl-do-one-event-loop) #+either-or (Tk_MainLoop)
  )

;; Our own event loop ! - Use this if it is desirable to do something
;; else between events

(defun tcl-do-one-event-loop ()
  (loop while (plusp (tk-get-num-main-windows))
      do (check-faux-events)
        (loop until (zerop (Tcl_DoOneEvent 2))) ;; 2== TCL_DONT_WAIT
        
      finally ;;(tk-eval "exit")
        (tcl-delete-interp *tki*)
        (setf *tki* nil)
        (trc "tcl-do-one-event-loop has left the building")))

(defparameter *event-loop-delay* 0.08 "Minimum delay [s] in event loop not to lock out IDE (ACL anyway)")

(let ((last-check nil)
      (check-interval (round (* 0.05 internal-time-units-per-second))))
  (defun check-faux-events ()
    (let ((now (get-internal-real-time)))
      (when (or (null last-check) (> (- now last-check) check-interval))
        (setf last-check now)
        (bwhen (events (tk-eval-list "set tk-events"))
          (tk-eval "set tk-events {}")
          (loop for e in events
                do (tk-process-event e))))
      (progn
        (trc nil "tcl-do-one-event-loop sees no events" (get-internal-real-time))
        (sleep *event-loop-delay*)))))

(defun tk-process-event (event)
  (trc nil "tk-process-event >" event *package*)
  (destructuring-bind (fn w-name &rest args)
      (let ((*package* (find-package :ctk)))
        (read-from-string (conc$ "(" event ")")))
    (let (#+nahh (id (symbol-name w-name)))
      (bif (w (gethash w-name (dictionary *tkw*)))
        (progn (trc nil "funcalling" fn w args)
          (apply fn w args))
        (progn
          (loop for k being the hash-keys of (dictionary *tkw*)
              do (trc "known key" k (symbol-package k)))
          (break "bad id ~a in event ~a" w-name event))))))

(defmethod do-on-event (self event-type$ &rest args &aux (event-type (intern event-type$ :ctk)))
  (assert (symbolp event-type))
  (trc nil "on event!!!" self event-type args)
  (bif (ecb (gethash event-type (event-handlers self)))
    (apply ecb self event-type args)
    (progn
      (trc "no event handlers for" self event-type (symbol-package event-type))
      (loop for k being the hash-keys of (event-handlers self)
              do (trc "known key" k (symbol-package k))))))

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