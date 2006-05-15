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
  ;; not recommended by Tcl doc (tcl-do-when-idle (get-callback 'tcl-idle-proc) 42)
  (tk-app-init *tki*)
  (tk-togl-init *tki*)
  (tk-format-now "proc TraceOP {n1 n2 op} {event generate $n1 <<tracewrite>> -data {$n1 $op}}")
  
  (with-integrity ()
    (setf *tkw* (make-instance root-class))

  (tk-create-event-handler-ex *tkw* 'main-window-proc :virtualEventMask))
    
  (tk-format `(:fini) "wm deiconify .")
  (tk-format-now "bind . <Escape> {destroy .}")

  ;; one or the other of...
 (tcl-do-one-event-loop)#+either-or   (Tk_MainLoop)
  )

(defcallback main-window-proc :void  ((client-data :int)(xe :pointer))
  (declare (ignore client-data))
  (when (eq (xevent-type xe) :virtualevent)  
    (bwhen (n$ (xsv name xe))
      (case (read-from-string (string-upcase n$))
        (do-menu-command (let ((self (gethash (tcl-get-string (xsv user-data xe)) (dictionary *tkw*))))
                           (bwhen (c (^on-command))
                             (funcall c self))))
        (time-is-up (let ((self (gethash (tcl-get-string (xsv user-data xe)) (dictionary *tkw*))))
                      (bwhen (c (^on-command))
                        (funcall c self))))
        (otherwise (trc "main window sees unknown" n$))))))

;; Our own event loop ! - Use this if it is desirable to do something
;; else between events

(defparameter *event-loop-delay* 0.08 "Minimum delay [s] in event loop not to lock out IDE (ACL anyway)")

(defun tcl-do-one-event-loop ()
  (loop while (plusp (tk-get-num-main-windows))
      do (loop until (zerop (Tcl_DoOneEvent 2))) ;; 2== TCL_DONT_WAIT
        (sleep *event-loop-delay*)
      finally ;;(tk-eval "exit")
        (tcl-delete-interp *tki*)
        (setf *tki* nil)))



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