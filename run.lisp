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

(in-package :celtk)



;;; --- running a Celtk (window class, actually) --------------------------------------

(eval-now!
 (export '(tk-scaling run-window test-window *ctk-dbg*)))

(defparameter *ctk-dbg* nil)

(defun run-window (root-class &optional (resetp t) &rest window-initargs)
  (declare (ignorable root-class))
  (setf *tkw* nil)
  (when resetp
    (cells-reset 'tk-user-queue-handler))

  (setf *tki* (Tcl_CreateInterp))
  ;; not recommended by Tcl doc (tcl-do-when-idle (get-callback 'tcl-idle-proc) 42)
  (tk-app-init *tki*)

  (tk-format-now "proc TraceOP {n1 n2 op} {event generate $n1 <<trace>> -data $op}")
  
  (tk-format-now "package require snack")
  (tk-format-now "package require tile")
  #-unix
  ;;(tk-format-now "package require QuickTimeTcl")
  (tk-format-now "snack::sound s")
;;;  (tk-format-now (conc$ "snack::sound s -load "
;;;                   (snackify-pathname (make-pathname :directory '(:absolute  "sounds")
;;;                                        :name "ahem_x" :type "wav")
;;;                     #+vs (car (directory (make-pathname :directory '(:absolute  "sounds")))))))
;;;  (tk-format-now "s play -blocking yes")
;;;  (sleep 2)
;;;  (tk-format-now "s play")

  (tcl-create-command *tki* "do-on-command" (get-callback 'do-on-command) (null-pointer) (null-pointer))

  ;; these next exist because of limitations in the Tcl API. eg, the keypress event does not
  ;; include enough info to extract the keysym directly, and the function to extract the
  ;; keysym is not exposed. The keysym, btw, is the portable representation of key events.

  (tcl-create-command *tki* "do-key-down" (get-callback 'do-on-key-down) (null-pointer) (null-pointer))
  (tcl-create-command *tki* "do-key-up" (get-callback 'do-on-key-up) (null-pointer) (null-pointer))

  (with-integrity () ;; w/i somehow ensures tkwin slot gets populated
    (setf *app*
      (make-instance 'application
        :kids (c? (the-kids
                   (setf *tkw* (apply 'make-instance root-class
                                 :fm-parent *parent*
                                 window-initargs))))
        )))
  
  (assert (tkwin *tkw*))
  
  (tk-format `(:fini) "wm deiconify .")
  (tk-format-now "bind . <Escape> {destroy .}")
  ;
  ; see above for why we are converting key x-events to application key virtual events:
  ;
  (tk-format-now "bind . <KeyPress> {do-key-down %W %K}")
  (tk-format-now "bind . <KeyRelease> {do-key-up %W %K}")
  (bwhen (ifn (start-up-fn *tkw*))
    (funcall ifn *tkw*))
  #+allegro (CG:kill-splash-screen)
  (tcl-do-one-event-loop)
  )



(defun ensure-destruction (w key)
  (declare (ignorable key))
  ;(TRC "ensure.destruction entry" key W (type-of w))
  (unless (find w *windows-being-destroyed*)
    ;(TRC "ensure.destruction not-to-being" key W)
    
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

(defmethod widget-event-handle ((self window) xe)
  (let ((*tkw* self))
    (unless (find (xevent-type xe) '(:MotionNotify))
      #+xxx (TRC "main window event" self *tkw* (xevent-type xe)))
    (flet ((give-to-window ()
             (bwhen (eh (event-handler *tkw*))
               (funcall eh *tkw* xe))))
      (case (xevent-type xe)
        ((:focusin :focusout) (setf (^focus-state) (xevent-type xe)))
        ((:MotionNotify :buttonpress)
         #+shhh (call-dump-event client-data xe))

        (:configurenotify
         (setf (^width) (parse-integer (tk-eval "winfo width .")))
         (with-cc :height
           (setf (^height) (parse-integer (tk-eval "winfo height ."))))
         )

        
        
        (:destroyNotify
         (pushnew *tkw* *windows-destroyed*)
         (ensure-destruction *tkw* :destroyNotify))

        (:virtualevent
         (bwhen (n$ (xsv name xe))
           (trc nil "main-window-proc :" n$ (unless (null-pointer-p (xsv user-data xe))
                                              (tcl-get-string (xsv user-data xe))))
           (case (read-from-string (string-upcase n$))
             (keypress ;(break "this works??: going after keysym")
               (let ((keysym (tcl-get-string (xsv user-data xe))))
                         (trc nil "keypress keysym!!!!" (tcl-get-string (xsv user-data xe)))
                         (bIf (mod (keysym-to-modifier keysym))
                           (eko (nil "modifiers now")
                             (pushnew mod (keyboard-modifiers *tkw*)))
                           (trc "unhandled pressed keysym" keysym))))
             (keyrelease (break "this works??: going after keysym")
               (let ((keysym (tcl-get-string (xsv user-data xe))))
                           (bIf (mod (keysym-to-modifier keysym))
                             (eko (nil "modifiers now")
                               (setf (keyboard-modifiers *tkw*)
                                 (delete mod (keyboard-modifiers *tkw*))))
                             (trc "unhandled released keysym" keysym))))
             (close-window
              (ensure-destruction *tkw* :close-window))
           
             (window-destroyed
              (ensure-destruction *tkw* :window-destroyed))
             
             (otherwise
              (give-to-window)))))
        (otherwise (give-to-window)))
      0)))

;; Our own event loop ! - Use this if it is desirable to do something
;; else between events

(defparameter *event-loop-delay*  .10 "Minimum delay [s] in event loop not to lock out IDE (ACL anyway)")

(defparameter *doe-last* 0)

(defun tcl-do-one-event-loop ()
  (app-idle-tasks-clear)
  (loop while (plusp (tk-get-num-main-windows))
      do (loop until (zerop (Tcl_DoOneEvent 2)) ;; 2== TCL_DONT_WAIT
             do (when (and *ctk-dbg* (> (- (now) *doe-last*) 1))
                    (trcx doe-loop)
                  (setf *doe-last* (now)))
               (app-idle *app*))
        (app-idle *app*)
        (sleep *event-loop-delay*) ;; give the IDE a few cycles
      finally
        (trc nil "Tcl-do-one-event-loop sees no more windows" *tki*)
        (tcl-delete-interp *tki*) ;; probably unnecessary
        (setf *app* nil *tkw* nil *tki* nil)))

(defmethod window-idle ((self window)))

(defun test-window (root-class &optional (resetp t) &rest window-initargs)
  "nails existing window as a convenience in iterative development"
  (declare (ignorable root-class))

  #+notquite (when (and *tkw* (fm-parent *tkw*)) ;; probably a better way to test if the window is still alive
    (not-to-be (fm-parent *tkw*))
    (setf *tkw* nil ctk::*app* nil))
  
  (apply 'run-celtk-window root-class  window-initargs))

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
             (progn
               (trc nil "defcommand > " ',^on-name self (cdr args))
               (apply ',do-on-name self (rest args)))
             (progn
               (break ",do-on-name> Target widget ~a does not exist" (car args))
               #+anyvalue? (tcl-set-result interp
                             (format nil ",do-on-name> Target widget ~a does not exist" (car args))
                             (null-pointer))
               1)))))))

(defcommand command)
;
; see notes elsewhere for why Tcl API deficiencies require augmented key handling via app virtual events
;
(defcommand key-down)
(defcommand key-up)

