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

(defctype tcl-retcode :int)

(defcenum tcl-retcode-values
  (:tcl-ok    0)
  (:tcl-error 1))
    
(defmethod translate-from-foreign (value (type (eql 'tcl-retcode)))
  (unless (eq value (foreign-enum-value 'tcl-retcode-values :tcl-ok))
    (error "Tcl error: ~a" (tcl-get-string-result *tki*)))
  value)
    
;; --- initialization ----------------------------------------

(defcfun ("Tcl_FindExecutable" tcl-find-executable) :void
  (argv0 :string))

(defcfun ("Tcl_Init" Tcl_Init) tcl-retcode
  (interp :pointer))

(defcfun ("Tk_Init" Tk_Init) tcl-retcode
  (interp :pointer))

(defcallback Tk_AppInit tcl-retcode
  ((interp :pointer))
  (unwind-protect
    (tk-app-init interp)))

(defun tk-app-init (interp)
  (assert interp)
  (Tcl_Init interp)
  (Tk_Init interp)
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
  (interp :pointer))

;;; --- windows ----------------------------------

(defcfun ("Tk_GetNumMainWindows" tk-get-num-main-windows) :int)
(defcfun ("Tk_MainWindow" tk-main-window) :pointer (interp :pointer))

(defcfun ("Tk_NameToWindow" tk-name-to-window) :pointer
  (interp :pointer)
  (pathName :string)
  (related-tkwin :pointer))

;;; --- eval -----------------------------------------------

(defcfun ("Tcl_EvalFile" %Tcl_EvalFile) tcl-retcode
  (interp        :pointer)
  (filename-cstr :string))
   
(defun Tcl_EvalFile (interp filename)
  (with-foreign-string (filename-cstr filename)
    (%Tcl_EvalFile interp filename-cstr)))

(defcfun ("Tcl_Eval" %Tcl_Eval) tcl-retcode
  (interp      :pointer)
  (script-cstr :string))

(defun tcl-eval (i s)
  (%Tcl_Eval i s))

(defcfun ("Tcl_EvalEx" %Tcl_EvalEx) tcl-retcode
  (interp      :pointer)
  (script-cstr :string)
  (num-bytes   :int)
  (flags       :int))

(defun tcl-eval-ex (i s)
  (%Tcl_EvalEx i s -1 0))

(defcfun ("Tcl_GetVar" tcl-get-var) :string
  (interp  :pointer)
  (varName :string)
  (flags   :int))

(defcfun ("Tcl_SetVar" tcl-set-var) :string
  (interp    :pointer)
  (var-name  :string)
  (new-value :string)
  (flags     :int))

(defcfun ("Tcl_GetStringResult" tcl-get-string-result) :string
  (interp :pointer))

;; ----------------------------------------------------------------------------
;; Tcl_CreateCommand - used to implement direct callbacks
;; ----------------------------------------------------------------------------

(defcfun ("Tcl_CreateCommand" tcl-create-command) :pointer
  (interp :pointer)
  (cmdName :string)
  (proc :pointer)
  (client-data :pointer)
  (delete-proc :pointer))

;; ----------------------------------------------------------------------------
;; Tcl/Tk channel related stuff
;; ----------------------------------------------------------------------------

(defcfun ("Tcl_RegisterChannel" Tcl_RegisterChannel) :void
  (interp :pointer)
  (channel :pointer))

(defcfun ("Tcl_UnregisterChannel" Tcl_UnregisterChannel) :void
  (interp :pointer)
  (channel :pointer))

(defcfun ("Tcl_MakeFileChannel" Tcl_MakeFileChannel) :pointer
  (handle :int)
  (readOrWrite :int))

(defcfun ("Tcl_GetChannelName" Tcl_GetChannelName) :string
  (channel :pointer))

(defcfun ("Tcl_GetChannelType" Tcl_GetChannelType) :pointer
  (channel :pointer))


(defcfun ("Tcl_GetChannel" Tcl_GetChannel) :pointer
  (interp :pointer)
  (channelName :string)
  (modePtr :pointer))

;; Send a script to a given Tcl/Tk interpreter

(defun eval-script (interp script)
  (assert interp)
  (assert script)
  (tcl-eval interp script))

