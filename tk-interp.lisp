(in-package :celtk)

;;------------------------------------------------------------------------------
;; GLOBAL VARS AND PARAMS
;;------------------------------------------------------------------------------


(defvar *tki* nil)
;;------------------------------------------------------------------------------
;; External LIBRARIES
;;------------------------------------------------------------------------------

#+handledelsewhere
(eval-when (:load-toplevel :compile-toplevel :execute)
  #+asdf (progn 
	   #-cffi (progn
		    (asdf:operate 'asdf:load-op :cffi)
	            (use-package :cffi))
	   #-cl-opengl (progn
		         (asdf:operate 'asdf:load-op :cl-opengl)
	                 (use-package :cl-opengl))
	   #-cells (progn
		         (asdf:operate 'asdf:load-op :cells)
	                 (use-package :cells))
	   )
  )

;; Eval-when :load-topvelel :execute needed to avoid multiple loads leading to
;; errors !
(eval-when (:load-toplevel :execute)
  (progn

    ;; Tcl/Tk
    
    (define-foreign-library Tcl
      (:darwin (:framework "Tcl"))
      (:windows (:or "/tcl/bin/Tcl84.dll")))
    (define-foreign-library Tk
      (:darwin (:framework "Tk"))
      (:windows (:or "/tcl/bin/tk84.dll")))
    
    ;; Togl

    (define-foreign-library Togl
      (:darwin (:or "/opt/tcltk/togl/lib/Togl1.7/libtogl1.7.dylib"))
      (:windows (:or "/tcl/lib/togl/togl17.dll")))
  )
)

(eval-when (:load-toplevel :compile :execute)
  (progn

    ;; Tcl Return Code Type

    (defctype tcl-retcode :int)

    (defcenum tcl-retcode-values
      (:tcl-ok    0)
      (:tcl-error 1))
    
    (defmethod translate-from-foreign (value (type (eql 'tcl-retcode)))
      (unless (eq value (foreign-enum-value 'tcl-retcode-values :tcl-ok))
        (error "*** Tcl error !"))
      value)
    
    ;; Tcl_FindExecutable

    (defcfun ("Tcl_FindExecutable" %Tcl_FindExecutable) :void
      (argv0 :string))

    (defun Tcl_FindExecutable ()
      (with-foreign-string (argv0-cstr (argv0))
	(%Tcl_FindExecutable argv0-cstr)))

    ;; Tcl_Init

    (defcfun ("Tcl_Init" %Tcl_Init) tcl-retcode
      (interp :pointer))

    (defun Tcl_Init (interp)
      (%Tcl_Init interp))

    ;; Tk_Init

    (defcfun ("Tk_Init" %Tk_Init) tcl-retcode
      (interp :pointer))

    (defun Tk_Init (interp)
      (%Tk_Init interp))

    ;; Tcl_SetVal

    (defcfun ("Tcl_SetVar" %Tcl_SetVar) :string
      (interp :pointer)
      (var-name :string)
      (new-value :string)
      (flags :int))

    (defun Tcl_SetVar (interp var-name new-value flags)
      (with-foreign-string (var-name-cstr var-name)
	(with-foreign-string (new-value-cstr new-value)
          (foreign-string-to-lisp
	    (%Tcl_SetVar interp var-name-cstr new-value-cstr flags)))))
    
    ;; Tcl_AppInit

    (defcallback Tk_AppInit tcl-retcode
      ((interp :pointer))
      (tk-app-init interp))

    (defun tk-app-init (interp)
      (Tcl_Init interp)
      (Tk_Init interp)

      ;; Initialize additional packages

      ;; Togl Initialization

      (Togl_Init interp)            
      
      ;; Say hello

      (format t "~%*** Tk_AppInit has been called.~%")

      ;; Return OK
      (foreign-enum-value 'tcl-retcode-values :tcl-ok)
    )

    ;; Tk_Main
    
    (defcfun ("Tk_MainEx" %Tk_MainEx) :void
      (argc :int)
      (argv :string)
      (Tk_AppInitProc :pointer)
      (interp :pointer))

    (defun Tk_Main ()
      (with-foreign-string (argv (argv0))
	(%Tk_MainEx 1 argv
		    (get-callback 'Tk_AppInit)
		    (Tcl_CreateInterp))))
    
    ;; Tcl_CreateInterp
      
    (defcfun ("Tcl_CreateInterp" %Tcl_CreateInterp) :pointer)

    (defun Tcl_CreateInterp ()
      (%Tcl_CreateInterp))

    ;; Tcl_EvalFile

    (defcfun ("Tcl_EvalFile" %Tcl_EvalFile) tcl-retcode
      (interp        :pointer)
      (filename-cstr :string))
   
    (defun Tcl_EvalFile (interp filename)
      (with-foreign-string (filename-cstr filename)
        (%Tcl_EvalFile interp filename-cstr)))

    ;; Tcl_Eval
    
    (defcfun ("Tcl_Eval" %Tcl_Eval) tcl-retcode
      (interp      :pointer)
      (script-cstr :pointer))

    (defcfun ("Tcl_GetStringResult" tcl-get-string-result) :string
      (interp      :pointer))

    (defcfun ("Tk_GetNumMainWindows" tk-get-num-main-windows) :int)
    
    (defun Tcl_Eval (interp script)
      (with-foreign-string (script-cstr script)
	(%Tcl_Eval interp script-cstr)))

    ;; Tcl_DoOneEvent

    (defcenum tcl-event-flag-values
      (:tcl-dont-wait         2)
      (:tcl-window-events     4)
      (:tcl-file-events       8)
      (:tcl-timer-events     16)
      (:tcl-idle-events      32)
      (:tcl-all-events       -3))
 
    (defcfun ("Tcl_DoOneEvent" %Tcl_DoOneEvent) :int
      (flags :int))

    (defun Tcl_DoOneEvent (flags)
      (%Tcl_DoOneEvent flags))

    ;; Tk_MainLoop

    (defcfun ("Tk_MainLoop" %Tk_MainLoop) :void)

    (defun Tk_MainLoop ()
      (%Tk_MainLoop))

    ;; Togl (Version 1.7 and above needed!

    ;; Togl_Init
    
    (defcfun ("Togl_Init" %Togl_Init) tcl-retcode
      (interp :pointer))

    (defun Togl_Init (interp)
      (%Togl_Init interp))

    ;; Togl_CreateFunc

    (defcfun ("Togl_CreateFunc" %Togl_CreateFunc) :void
      (togl-callback-ptr :pointer))

    (defun Togl_CreateFunc (create-callback-ptr)
      (%Togl_CreateFunc create-callback-ptr))

    ;; Togl_DisplayFunc

    (defcfun ("Togl_DisplayFunc" %Togl_DisplayFunc) :void
      (togl-callback-ptr :pointer))

    (defun Togl_DisplayFunc (display-callback-ptr)
      (%Togl_DisplayFunc display-callback-ptr))

    ;; Togl_ReshapeFunc

    (defcfun ("Togl_ReshapeFunc" %Togl_ReshapeFunc) :void
      (togl-callback-ptr :pointer))

    (defun Togl_ReshapeFunc (reshape-callback-ptr)
      (%Togl_ReshapeFunc reshape-callback-ptr))

    ;; Togl_DestroyFunc

    (defcfun ("Togl_DestroyFunc" %Togl_DestroyFunc) :void
      (togl-callback-ptr :pointer))

    (defun Togl_DestroyFunc (destroy-callback-ptr)
      (%Togl_DestroyFunc destroy-callback-ptr))

    ;; Togl_TimerFunc

    (defcfun ("Togl_TimerFunc" %Togl_TimerFunc) :void
      (togl-callback-ptr :pointer))

    (defun Togl_TimerFunc (timer-callback-ptr)
      (%Togl_TimerFunc timer-callback-ptr))

    ;; Togl_PostRedisplay

    (defcfun ("Togl_PostRedisplay" %Togl_PostRedisplay) :void
      (togl-struct-ptr :pointer))

    (defun Togl_PostRedisplay (togl-struct-ptr)
      (%Togl_PostRedisplay togl-struct-ptr))

    ;; Togl_SwapBuffers

    (defcfun ("Togl_SwapBuffers" %Togl_SwapBuffers) :void
      (togl-struct-ptr :pointer))

    (defun Togl_SwapBuffers (togl-struct-ptr)
      (%Togl_SwapBuffers togl-struct-ptr))

    ;; Togl_Ident

    (defcfun ("Togl_Ident" %Togl_Ident) :string
      (togl-struct-ptr :pointer))

    (defun Togl_Ident (togl-struct-ptr)
      (foreign-string-to-lisp (%Togl_Ident togl-struct-ptr)))

    ;; Togl_Width

    (defcfun ("Togl_Width" %Togl_Width) :int
      (togl-struct-ptr :pointer))

    (defun Togl_Width (togl-struct-ptr)
      (%Togl_Width togl-struct-ptr))

    ;; Togl_Width

    (defcfun ("Togl_Height" %Togl_Height) :int
      (togl-struct-ptr :pointer))

    (defun Togl_Height (togl-struct-ptr)
      (%Togl_Height togl-struct-ptr))

    ;; Togl_Interp

    (defcfun ("Togl_Interp" %Togl_Interp) :pointer
      (togl-struct-ptr :pointer))

    (defun Togl_Interp (togl-struct-ptr)
      (%Togl_Interp togl-struct-ptr))

    ;; Togl_AllocColor
    ;; Togl_FreeColor

    ;; Togl_LoadBitmapFont
    ;; Togl_UnloadBitmapFont

    ;; Togl_SetClientData
    ;; Togl_ClientData

    ;; Togl_UseLayer
    ;; Togl_ShowOverlay
    ;; Togl_HideOverlay
    ;; Togl_PostOverlayRedisplay
    ;; Togl_OverlayDisplayFunc
    ;; Togl_ExistsOverlay
    ;; Togl_GetOverlayTransparentValue
    ;; Togl_IsMappedOverlay
    ;; Togl_AllocColorOverlay
    ;; Togl_FreeColorOverlay

    ;; Togl_DumpToEpsFile
  )
)

;; Initialization mgmt - required to avoid multiple library loads

(defvar *initialized* nil)

(defun set-initialized ()
  (setq *initialized* t))

(defun reset-initialized ()
  (setq *initialized* nil))

(defun argv0 ()
  (sys:command-line-argument 0))  ;; frgo: PORTING: This only works in ACL

(defun tk-interp-init-ensure ()
  (unless *initialized*
    (use-foreign-library Tcl)
    (use-foreign-library Tk)
    (use-foreign-library Togl)
    (prog1
      (Tcl_FindExecutable)
      (set-initialized))))

;; Send a script to a piven Tcl/Tk interpreter

(defun eval-script (interp script)
  (assert interp)
  (assert script)

  (Tcl_Eval interp script))



;;; Main - our test function
;;;
;;;(defun main (file)
;;;  (setf *start-time* (get-internal-real-time))
;;;
;;;  (tk-interp-init-ensure)
;;;
;;;  (let ((interp (Tcl_CreateInterp)))
;;;    (tk-app-init interp)
;;;    (togl-init interp)
;;;    (Tcl_EvalFile interp file))
;;;
;;;  (Tk_MainLoop)
;;;)

(defun exec-button ()
  (tk-interp-init-ensure)
  (let ((interp (Tcl_CreateInterp)))
    (tk-app-init interp)
    (togl-init interp)
    #+works (progn
              (eval-script interp "button .b1 -text Hello")
              (eval-script interp "pack .b1"))
    (eval-script interp "togl .t1 -height 100 -height 100 -ident t1")
    ;;(eval-script interp "puts \"Hello puts\"")
    )
  (Tk_MainLoop))

(defun test-result ()
  (tk-interp-init-ensure)
  (let ((*tki* (Tcl_CreateInterp)))
    (tk-app-init *tki*)
    #+wait (eval-script *tki* "font families")
    #+ok (eval-script *tki* "tk scaling")
    #+ok (progn
      (eval-script *tki* "set xyz 42")
      (eval-script *tki* "set xyz"))
    ;;(trc "string result:" (tcl-get-string-result interp))
    (trc "tk-eval result:" (tk-eval "tk scaling"))
    (trc "tk-eval-list result:" (tk-eval-list "font families"))))

;;;(defun exec-main ()
;;;  (main "\\0devtools\\frgotk\\psu-rc-gui.tcl"))
;;;
;;;#+test
;;;(exec-main)

;;; Togl stuff

(defparameter *togl-initialized* nil
  "Flag, t if Togl is considered initialized")

;; Callbacks, global

(defctype togl-struct-ptr-type :pointer)

;; These will be called from C land

;; CREATE CALLBACK

(defparameter *togl-create-cb-address* nil)

(defcallback %%togl-create-cb :void ((togl-struct-ptr togl-struct-ptr-type))
  (%togl-create-cb togl-struct-ptr))

;(eval-when (:load-toplevel :compile-toplevel :execute)
;  (setq *togl-create-cb-address* (get-callback '%%togl-create-cb)))

(defun %togl-create-cb (togl-struct-ptr)
  (declare (ignorable togl-struct-ptr))
  (format t "~&*** TOGL CREATE CB has been called.~&"))

;; DISPLAY CALLBACK

(defparameter *togl-display-cb-address* nil)

(defcallback %%togl-display-cb :void ((togl-struct-ptr togl-struct-ptr-type))
  (%togl-display-cb togl-struct-ptr))

;(eval-when (:load-toplevel :compile-toplevel :execute)
;  (setq *togl-display-cb-address* (get-callback '%%togl-display-cb)))

(defun %togl-display-cb (togl-struct-ptr)
  (declare (ignorable togl-struct-ptr))
  (format t "~&*** TOGL DISPLAY CB has been called.~&"))

;; RESHAPE CALLBACK

(defparameter *togl-reshape-cb-address* nil)

(defcallback %%togl-reshape-cb :void ((togl-struct-ptr togl-struct-ptr-type))
   (%togl-reshape-cb togl-struct-ptr))

;(eval-when (:load-toplevel :compile-toplevel :execute)
;  (setq *togl-reshape-cb-address* (get-callback '%%togl-reshape-cb)))

(defun %togl-reshape-cb (togl-struct-ptr)
  (declare (ignorable togl-struct-ptr))
  (format t "~&*** TOGL RESHAPE CB has been called.~&"))

;; DESTROY CALLBACK

(defparameter *togl-destroy-cb-address* nil)

(defcallback %%togl-destroy-cb :void ((togl-struct-ptr togl-struct-ptr-type))
  (%togl-destroy-cb togl-struct-ptr))

;(eval-when (:load-toplevel :compile-toplevel :execute)
;  (setq *togl-destroy-cb-address* (get-callback '%%togl-destroy-cb)))

(defun %togl-destroy-cb (togl-struct-ptr)
  (declare (ignorable togl-struct-ptr))
  (format t "~&*** TOGL DESTROY CB has been called.~&"))

;; TIMER CALLBACK

(defparameter *togl-timer-cb-address* nil)

(defcallback %%togl-timer-cb :void ((togl-struct-ptr togl-struct-ptr-type))
  (%togl-timer-cb togl-struct-ptr))

;(eval-when (:load-toplevel :compile-toplevel :execute)
;  (setq *togl-timer-cb-address* (get-callback '%%togl-timer-cb)))

(defun %togl-timer-cb (togl-struct-ptr)
  (declare (ignorable togl-struct-ptr))
  (format t "~&*** TOGL TIMER CB has been called.~&"))

;; Initialization
(defun togl-init (interp)

  (assert interp)
  
  (unless *togl-initialized*

    (Togl_Init interp)

    (Togl_CreateFunc  (get-callback '%%togl-create-cb))
    (Togl_DisplayFunc (get-callback '%%togl-display-cb))
    (Togl_ReshapeFunc (get-callback '%%togl-reshape-cb))
    (Togl_DestroyFunc (get-callback '%%togl-destroy-cb))
    (Togl_TimerFunc   (get-callback '%%togl-timer-cb))

    (setq *togl-initialized* t)))



