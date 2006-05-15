(in-package :celtk)

;;------------------------------------------------------------------------------
;; GLOBAL VARS AND PARAMS
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; External LIBRARIES
;;------------------------------------------------------------------------------

#+FRANKG
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


;; Tcl/Tk

;;;(define-foreign-library Tcl
;;;    (:darwin (:framework "Tcl"))
;;;  (:windows (:or "d:/tcl/bin/Tcl84.dll")))
;;;(define-foreign-library Tk
;;;    (:darwin (:framework "Tk"))
;;;  (:windows (:or "d:/tcl/bin/tk84.dll")))
    
(define-foreign-library Tcl
    (:darwin (:framework "Tcl"))
  (:windows (:or "/tcl/bin/Tcl85.dll")))
(define-foreign-library Tk
    (:darwin (:framework "Tk"))
  (:windows (:or "/tcl/bin/tk85.dll")))
    
;; Togl
(define-foreign-library Togl
    (:darwin (:or "/opt/tcltk/togl/lib/Togl1.7/libtogl1.7.dylib"))
  (:windows (:or "/tcl/lib/togl/togl17.dll")))

(defctype tcl-retcode :int)

(defcenum tcl-retcode-values
    (:tcl-ok    0)
  (:tcl-error 1))
    
(defmethod translate-from-foreign (value (type (eql 'tcl-retcode)))
  (unless (eq value (foreign-enum-value 'tcl-retcode-values :tcl-ok))
    (error "Tcl error: ~a" (tcl-get-string-result *tki*)))
  value)
    
;; --- initialization ----------------------------------------

(defcfun ("Tcl_FindExecutable" %Tcl_FindExecutable) :void
  (argv0 :string))

(defun Tcl_FindExecutable ()
  (with-foreign-string (argv0-cstr (argv0))
    (%Tcl_FindExecutable argv0-cstr)))

;; Tcl_Init

(defcfun ("Tcl_Init" Tcl_Init) tcl-retcode
  (interp :pointer))

;; Tk_Init

(defcfun ("Tk_Init" Tk_Init) tcl-retcode
  (interp :pointer))

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

(defcallback Tk_AppInit tcl-retcode
  ((interp :pointer))
  (tk-app-init interp))
  
;; Tcl_AppInit

(defun tk-app-init (interp)
  (Tcl_Init interp)
  (Tk_Init interp)

  (format t "~%*** Tk_AppInit has been called.~%")

  ;; Return OK
  (foreign-enum-value 'tcl-retcode-values :tcl-ok))

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

(defcfun ("Tcl_CreateInterp" Tcl_CreateInterp) :pointer)

(defcfun ("Tcl_DeleteInterp" tcl-delete-interp) :void
  (interp        :pointer))

;; Tcl_EvalFile

(defcfun ("Tcl_EvalFile" %Tcl_EvalFile) tcl-retcode
  (interp        :pointer)
  (filename-cstr :string))
   
(defun Tcl_EvalFile (interp filename)
  (with-foreign-string (filename-cstr filename)
    (%Tcl_EvalFile interp filename-cstr)))

(defcfun ("Tcl_Eval" tcl-eval) tcl-retcode
  (interp      :pointer)
  (script-cstr :string))

(defcfun ("Tcl_EvalEx" tcl_evalex) tcl-retcode
  (interp      :pointer)
  (script-cstr :string)
  (num-bytes :int)
  (flags :int))

(defun tcl-eval-ex (i s)
  (tcl_evalex i s -1 0))

(defcfun ("Tcl_GetStringResult" tcl-get-string-result) :string
  (interp      :pointer))

(defcfun ("Tk_GetNumMainWindows" tk-get-num-main-windows) :int)
(defcfun ("Tk_MainWindow" tk-main-window) :pointer (interp :pointer))

(defcfun ("Tk_NameToWindow" tk-name-to-window) :pointer
  (interp :pointer)
  (pathName :string)
  (related-tkwin :pointer))



;;; --- Togl (Version 1.7 and above needed!) -----------------------------

   
(defcfun ("Togl_Init" Togl_Init) tcl-retcode
  (interp :pointer))

(defcfun ("Togl_CreateFunc" Togl_CreateFunc) :void
  (togl-callback-ptr :pointer))

(defcfun ("Togl_DisplayFunc" Togl_DisplayFunc) :void
  (togl-callback-ptr :pointer))

(defcfun ("Togl_ReshapeFunc" Togl_ReshapeFunc) :void
  (togl-callback-ptr :pointer))

(defcfun ("Togl_DestroyFunc" Togl_DestroyFunc) :void
  (togl-callback-ptr :pointer))

(defcfun ("Togl_TimerFunc" Togl_TimerFunc) :void
  (togl-callback-ptr :pointer))

(defcfun ("Togl_PostRedisplay" Togl_PostRedisplay) :void
  (togl-struct-ptr :pointer))

(defcfun ("Togl_SwapBuffers" Togl_SwapBuffers) :void
  (togl-struct-ptr :pointer))

(defcfun ("Togl_Ident" Togl-Ident) :string
  (togl-struct-ptr :pointer))

(defcfun ("Togl_Width" Togl_Width) :int
  (togl-struct-ptr :pointer))

(defcfun ("Togl_Height" Togl_Height) :int
  (togl-struct-ptr :pointer))

(defcfun ("Togl_Interp" Togl_Interp) :pointer
  (togl-struct-ptr :pointer))

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


;; Initialization mgmt - required to avoid multiple library loads

(defvar *initialized* nil)

(defun set-initialized ()
  (setq *initialized* t))

(defun reset-initialized ()
  (setq *initialized* nil))

(defun argv0 ()
  #+allegro (sys:command-line-argument 0)
  #+lispworks (nth 0 (io::io-get-command-line-arguments)))

(defun tk-interp-init-ensure ()
  (unless *initialized*
    (use-foreign-library Tcl)
    (use-foreign-library Tk)
    (use-foreign-library Togl)
    (Tcl_FindExecutable)
    (set-initialized)))

;; Send a script to a piven Tcl/Tk interpreter

(defun eval-script (interp script)
  (assert interp)
  (assert script)

  (tcl-eval interp script))

#+testing
(defun exec-button ()
  (tk-interp-init-ensure)
  (let ((interp (Tcl_CreateInterp)))
    (tk-app-init interp)
    (togl_init interp)
    #+works (progn
              (eval-script interp "button .b1 -text Hello")
              (eval-script interp "pack .b1"))
    (eval-script interp "togl .t1 -height 100 -height 100 -ident t1")
    ;;(eval-script interp "puts \"Hello puts\"")
    )
  (Tk_MainLoop))

#+testing
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



