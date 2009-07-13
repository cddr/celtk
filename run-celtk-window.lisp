(in-package :celtk)

(defun celtk-reset ()
  (setf *tkw* nil)
  (cells-reset 'tk-user-queue-handler))

(defun celtk-init (window-class initargs)

  (trc "init args> " initargs)
  (let ((*tki* (Tcl_CreateInterp)))
    (tk-app-init *tki*)
    (tk-format-now "proc TraceOP {n1 n2 op} {
                      event generate $n1 <<trace>> -data $op
                    }")
  
    (tk-format-now "package require snack")
    (tk-format-now "package require tile")
    
    (flet ((add-command (str symbol)
	     (tcl-create-command *tki*
               str
	       (get-callback symbol)
	       (null-pointer)
	       (null-pointer))))

      (add-command "do-on-command" 'do-on-command)
      (add-command "do-key-down" 'do-on-key-down)
      (add-command "do-key-up" 'do-on-key-up))

    (wrap-window window-class initargs)

    *tki*))

(defun celtk-teardown ()
  (tcl-delete-interp *tki*)
  (setf *app* nil
	*tkw* nil
	*tki* nil))

(defun wrap-window (window-class initargs)
  (macrolet ((mk-window (window-class initargs)
	       `(apply 'make-instance ,window-class
		         :fm-parent *parent* 
		         ,initargs))
	     (mk-app (win)
	       `(make-instance 'application
		 :kids (c? (the-kids
			    (setf *tkw* ,win))))))
    (with-integrity ()
      (setf *app*
	    (mk-app (mk-window window-class
			       initargs))))

    (assert (tkwin *tkw*))

    (tk-format `(:fini) "wm deiconify .")

    (tk-format-now "bind . <Escape> {destroy .}")
    (tk-format-now "bind . <KeyPress> {do-key-down %W %K}")
    (tk-format-now "bind . <KeyRelease> {do-key-up %W %K}")
    
    (bwhen (ifn (start-up-fn *tkw*))
      (funcall ifn *tkw*))))

(defun run-celtk-window (window-class &rest window-initargs)
  ;; How cool is loop?!! Our entire event framework is
  ;; encapsulated in a single loop
  (trc "init args> " window-initargs)
  (flet ((process-event ()
	   (app-idle-tasks-clear)
	   (loop
	      until (zerop (Tcl_DoOneEvent 2))
	      do (when (and *ctk-dbg*
			    (> (- (now) *doe-last*) 1))
		   ;(trcx doe-loop)
		   (setf *doe-last* (now)))
		(app-idle *app*))))
    (loop
       initially (progn
		   (celtk-reset)
		   (setf *tki* (celtk-init window-class 
					   window-initargs))
		   (app-idle-tasks-clear))
       while (plusp (tk-get-num-main-windows))
       do (progn
	    (process-event)
	    (app-idle *app*)
	    (sleep *event-loop-delay*))
       finally
	 (celtk-teardown))))
	 
