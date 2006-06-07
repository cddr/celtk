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

;;; --- running a Celtk (window class, actually) --------------------------------------

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
  (tcl-create-command *tki* "do-on-command" (get-callback 'do-on-command) (null-pointer) (null-pointer))
  (tcl-create-command *tki* "do-key-down" (get-callback 'do-on-key-down) (null-pointer) (null-pointer))
  (tcl-create-command *tki* "do-key-up" (get-callback 'do-on-key-up) (null-pointer) (null-pointer))

  (with-integrity () ;; w/i somehow ensures tkwin slot gets populated
    (setf *app*
      (make-instance 'application
        :kids (c? (the-kids
                   (setf *tkw* (make-instance root-class
                                 :fm-parent *parent*)))))))

  (assert (tkwin *tkw*))

  (tk-create-event-handler-ex *tkw* 'main-window-proc -1)
  
  (tk-format `(:fini) "wm deiconify .")
  (tk-format-now "bind . <Escape> {destroy .}")
  (tk-format-now "bind . <KeyPress> {do-key-down %W %K}")
  (tk-format-now "bind . <KeyRelease> {do-key-up %W %K}")
  (tcl-do-one-event-loop))

(defun ensure-destruction (w)
  (TRC nil "ensure-destruction entry" W)
  (unless (find w *windows-being-destroyed*)
    (TRC nil "ensure-destruction not-to-being" W)
    (let ((*windows-being-destroyed* (cons w *windows-being-destroyed*)))
      (not-to-be w))))

(defparameter *keyboard-modifiers*
  (loop with km = (make-hash-table :test 'equalp)
      for (keysym mod) in '(("Shift_L" :shift)
                          ("Shift_R" :shift)
                          ("Alt_L" :alt)
                          ("Alt_R" :alt)
                          ("Control_L" :control)
                          ("Control_R" :control))
      do (setf (gethash keysym km) mod)
      finally (return km)))

(defun keysym-to-modifier (keysym)
  (gethash keysym *keyboard-modifiers*))

(defcallback main-window-proc :void  ((client-data :pointer)(xe :pointer))
  (let ((*tkw* (tkwin-widget client-data)))
    (assert (typep *tkw* 'window))
    (TRC nil "main window event" (xevent-type xe))
    (flet ((give-to-window ()
             (bwhen (eh (event-handler *tkw*))
               (funcall eh *tkw* xe))))
      (case (xevent-type xe)
        ((:MotionNotify :buttonpress)
         #+shhh (call-dump-event client-data xe))
        (:destroyNotify
         (let ((*windows-destroyed* (cons *tkw* *windows-destroyed*)))
           (ensure-destruction *tkw*)))
        (:virtualevent
         (bwhen (n$ (xsv name xe))
           (trc nil "main-window-proc :" n$ (unless (null-pointer-p (xsv user-data xe))
                                              (tcl-get-string (xsv user-data xe))))
           (case (read-from-string (string-upcase n$))
             (keypress (let ((keysym (tcl-get-string (xsv user-data xe))))
                         (bIf (mod (keysym-to-modifier keysym))
                           (eko ("modifiers now")
                             (pushnew mod (keyboard-modifiers *tkw*)))
                           (trc "unhandled pressed keysym" keysym))))
             (keyrelease (let ((keysym (tcl-get-string (xsv user-data xe))))
                           (bIf (mod (keysym-to-modifier keysym))
                             (eko ("modifiers now")
                               (setf (keyboard-modifiers *tkw*)
                                 (delete mod (keyboard-modifiers *tkw*))))
                             (trc "unhandled released keysym" keysym))))
             (close-window
              (ensure-destruction *tkw*))
           
             (window-destroyed
              (ensure-destruction *tkw*))
             
             (otherwise (give-to-window)))))
        (otherwise (give-to-window)))
      0)))

;; Our own event loop ! - Use this if it is desirable to do something
;; else between events

(defparameter *event-loop-delay* 0.08 "Minimum delay [s] in event loop not to lock out IDE (ACL anyway)")

(defun tcl-do-one-event-loop ()
  (loop while (plusp (tk-get-num-main-windows))
      do (loop until (zerop (Tcl_DoOneEvent 2))
             do (app-idle *app*)) ;; 2== TCL_DONT_WAIT
        (app-idle *app*)
        (sleep *event-loop-delay*) ;; give the IDE a few cycles
      finally
        (trc nil "Tcl-do-one-event-loop sees no more windows" *tki*)
        (tcl-delete-interp *tki*) ;; probably unnecessary
        (setf *app* nil *tkw* nil *tki* nil)))

(defmethod window-idle ((self window)))

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

;;; --- commands -----------------------------------------------------------------

(defmacro defcommand (name)
  (let ((do-on-name (read-from-string (format nil "DO-ON-~a" name)))
        (^on-name (read-from-string (format nil "^ON-~a" name))))
    `(progn
       (defmethod ,do-on-name (self &rest args)
         (bwhen (cmd (,^on-name))
           (apply cmd self args))
         0)

       (defcallback ,do-on-name :int ((client-data :pointer)(interp :pointer)(argc :int)(argv :pointer))
         (declare (ignore client-data))
         (let ((*tki* interp)
               (args (loop for argn upfrom 1 below argc
                         collecting (mem-aref argv :string argn))))
           (bif (self (gethash (car args) (dictionary *tkw*)))
             (apply ',do-on-name self (rest args))
             (progn
               (break ",do-on-name> Target widget ~a does not exist" (car args))
               #+anyvalue? (tcl-set-result interp
                             (format nil ",do-on-name> Target widget ~a does not exist" (car args))
                             (null-pointer))
               1)))))))

(defcommand command)
(defcommand key-up)
(defcommand key-down)

