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
  (tk-format-now "proc TraceOP {n1 n2 op} {event generate $n1 <<trace>> -data $op}")
  (tcl-create-command *tki* "do-on-command" (get-callback 'do-on-command)  (null-pointer) (null-pointer))
  
  (with-integrity ()
    (setf *tkw* (make-instance root-class))

  (tk-create-event-handler-ex *tkw* 'main-window-proc :structureNotifyMask :virtualEventMask))
  
  (tk-format `(:fini) "wm deiconify .")
  (tk-format-now "bind . <Escape> {destroy .}")
  (tk-format-now "bind . <Destroy> {event generate . <<window-destroyed>>}")

  (tcl-do-one-event-loop))

(defun ensure-destruction (w)
  (unless (find w *windows-being-destroyed*)
    (let ((*windows-being-destroyed* (cons w *windows-being-destroyed*)))
      (not-to-be w))))

(defcallback main-window-proc :void  ((client-data :pointer)(xe :pointer))
  (declare (ignore client-data))
  (TRC nil "main window event" (xevent-type xe))
  (case (xevent-type xe)
    (:destroyNotify
     (let ((*windows-destroyed* (cons *tkw* *windows-destroyed*)))
       (ensure-destruction *tkw*)))
    (:virtualevent
     (bwhen (n$ (xsv name xe))
       (case (read-from-string (string-upcase n$))

         (close-window
          (ensure-destruction *tkw*))

         (window-destroyed
          (ensure-destruction *tkw*))

         (time-is-up
          (let ((self (gethash (tcl-get-string (xsv user-data xe)) (dictionary *tkw*))))
            (bwhen (c (^on-command))
              (funcall c self))))

         (otherwise (trc "main window sees unknown" n$)))))))

;; Our own event loop ! - Use this if it is desirable to do something
;; else between events

(defparameter *event-loop-delay* 0.08 "Minimum delay [s] in event loop not to lock out IDE (ACL anyway)")

(defun tcl-do-one-event-loop ()
  (loop while (progn (trc "checking num main windows")
                (plusp (tk-get-num-main-windows)))
      do (trc "calling Tcl_DoOneEvent" (tk-get-num-main-windows))
        (loop until (zerop (Tcl_DoOneEvent 2))) ;; 2== TCL_DONT_WAIT
        (trc "sleeping")
        (sleep *event-loop-delay*) ;; give the IDE a few cycles
      finally
        (trc "Tcl-do-one-event-loop sees no more windows" *tki*)
        (tcl-delete-interp *tki*) ;; probably unnecessary
        (setf *tki* nil)))

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
