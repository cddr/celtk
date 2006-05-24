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

;;;(defctype tcl-retcode :int)
;;;
;;;(defcenum tcl-retcode-values
;;;    (:tcl-ok    0)
;;;  (:tcl-error 1))
;;;    
;;;(defmethod translate-from-foreign (value (type (eql 'tcl-retcode)))
;;;  (unless (eq value (foreign-enum-value 'tcl-retcode-values :tcl-ok))
;;;    (error "*** Tcl error !"))
;;;  value)
;;;
;;;(define-foreign-library Tcl
;;;    (:windows "/tcl/bin/Tcl84.dll")
;;;  (:darwin (:framework "Tcl")))
;;;
;;;(define-foreign-library Tk
;;;    (:windows "/tcl/bin/Tk84.dll")
;;;  (:darwin (:framework "Tk")))
;;;
;;;(defcfun ("Tcl_InitStubs" tcl-init-stubs) :int
;;;  (interp :pointer)(version :string)(math-version-exactly :int))
;;;
;;;(defcfun ("Tk_InitStubs" tk-init-stubs) :int
;;;  (interp :pointer)(version :string)(math-version-exactly :int))
;;;
;;;(defcfun ("Togl_Init" togl-init) tcl-retcode
;;;  (interp :pointer))

(eval-when (compile load eval)
  (export '(togl_swapbuffers togl_postredisplay togl-ptr togl-reshape-func
             togl togl-timer-using-class Togl_PostRedisplay togl-reshape-using-class
             togl-display-using-class togl_width togl_height togl-create-using-class)))

;; --- gotta call this bad boy during initialization, I guess any time after we have an interpreter
;;

(defun tk-togl-init (interp)
  ;(assert (not (zerop (tcl-init-stubs interp "8.1" 0))))
  ;(assert (not (zerop (tk-init-stubs interp "8.1" 0))))
  (togl_init interp)
  (togl-create-func (callback togl-create))
  ;;; needed? (togl-destroy-func (callback togl-destroy)
  (togl-display-func (callback togl-display))
  (togl-reshape-func (callback togl-reshape))
  (togl-timer-func (callback togl-timer)) ;; probably want to make this optional
  )

(deftk togl (widget)
  ((togl-ptr :cell nil :initform nil :initarg :togl-ptr :accessor togl-ptr)
   (cb-create :initform nil :initarg :cb-create :reader cb-create)
   (cb-display :initform nil :initarg :cb-display :reader cb-display)
   (cb-reshape :initform nil :initarg :cb-reshape :reader cb-reshape)
   (cb-destroy :initform nil :initarg :cb-destroy :reader cb-destroy)
   (cb-timer :initform nil :initarg :cb-timer :reader cb-timer))
  (:tk-spec togl
    -width ;;		400	Width of widget in pixels.
    -height ;;		400	Height of widget in pixels.
    -ident	;;	""	A user identification string ignored by togl.
		;;	This can be useful in your C callback functions
		;;	to determine which Togl widget is the caller.
    -rgba	;;	true	If true, use RGB(A) mode
		;;	If false, use Color Index mode
    -redsize      ;;	1	Min bits per red component
    -greensize	;; 1	Min bits per green component
    -bluesize	;; 1	Min bits per blue component
    -double		;; false	If false, request a single buffered window
			;; If true, request double buffered window
    -depth		;; false	If true, request a depth buffer
    -depthsize	;; 1	Min bits of depth buffer
    -accum		;; false	If true, request an accumulation buffer
    -accumredsize	;; 1	Min bits per accum red component
    -accumgreensize	;; 1	Min bits per accum green component
    -accumbluesize	;; 1	Min bits per accum blue component
    -accumalphasize	;; 1	Min bits per accum alpha component
    -alpha		;; false	If true and -rgba is true, request an alpha
			;; channel
    -alphasize	;; 1	Min bits per alpha component
    -stencil	;; false	If true, request a stencil buffer
    -stencilsize	;; 1	Min number of stencil bits
    -auxbuffers	;; 0	Desired number of auxiliary buffers
    -privatecmap	;; false	Only applicable in color index mode.
		 	;; If false, use a shared read-only colormap.
			;; If true, use a private read/write colormap.
    -overlay      ;; false   If true, request overlay planes.
    -stereo       ;; false   If true, request a stereo-capable window.
    (-timer-interval -time)  ;; 1       Specifies the interval, in milliseconds, for
                  ;     calling the C timer callback function which
                  ;    was registered with Togl_TimerFunc.
    -sharelist    ;; ""      Name of an existing Togl widget with which to
                  ;     share display lists.
                  ;    NOT YET IMPLEMENTED FOR WINDOWS 95/NT.
    -sharecontext ;; ""      Name of an existing Togl widget with which to
                  ;     share the OpenGL context.  NOTE:  most other
                  ;    attributes such as double buffering, RGBA vs CI,
                  ;   ancillary buffer specs, etc are then ignored.
                  ;  NOT YET IMPLEMENTED FOR WINDOWS 95/NT.
    -indirect     ;; false   If present, request an indirect rendering context.
                  ;     A direct rendering context is normally requested.
                  ;    NOT SIGNIFICANT FOR WINDOWS 95/NT.
    )
  (:default-initargs
      :id (gentemp "TOGL")
    :ident (c? (^path))))

(defmacro def-togl-callback (root (&optional (ptr-var 'togl-ptr)(self-var 'self)) &body preamble)
  (let ((register$ (format nil "TOGL-~a-FUNC" root))
        (cb$ (format nil "TOGL-~a" root))
        (cb-slot$ (format nil "CB-~a" root))
        (uc$ (format nil "TOGL-~a-USING-CLASS" root)))
    `(progn
       (defcfun (,(format nil "Togl_~:(~a~)Func" root) ,(intern register$))
           :void
         (callback :pointer))
       (defcallback ,(intern cb$) :void ((,ptr-var :pointer))
         (unless (c-stopped)
           (let ((,self-var (or (gethash (pointer-address ,ptr-var) (tkwins *tkw*))
                              (gethash (togl-ident ,ptr-var)(dictionary *tkw*)))))
             ,@preamble
             (,(intern uc$) ,self-var))))
       (defmethod ,(intern uc$) :around ((self togl))
         (if (,(intern cb-slot$) self)
               (funcall (,(intern cb-slot$) self) self)
             (call-next-method)))
       (defmethod ,(intern uc$) ((self togl))))))

(def-togl-callback create ()
    (setf (togl-ptr self) togl-ptr))
(def-togl-callback display ())
(def-togl-callback reshape ())
(def-togl-callback destroy ())
(def-togl-callback timer ())
#+not
(defmethod togl-timer-using-class :after ((self togl))
  (loop until (zerop (ctk::Tcl_DoOneEvent 2))))
       
(defmethod make-tk-instance ((self togl))
  (with-integrity (:client `(:make-tk ,self))
    (setf (gethash (^path) (dictionary .tkw)) self)
    (tk-format-now "togl ~a ~{~(~a~) ~a~^ ~}"
      (path self)(tk-configurations self)))) ;; this leads to "togl <path> [-<config option> <value]*", in turn to togl_create

