;; -*- mode: Lisp; Syntax: Common-Lisp; Package: celtk; -*-
;;;
;;; Copyright (c) 2006 by Frank Goenninger, Germany.
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in 
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;;
;;; ---------------------------------------------------------------------------
;;; $Header$
;;; ---------------------------------------------------------------------------

;;; ===========================================================================
;;; PACKAGE / EXPORTS
;;; ===========================================================================

(in-package :celtk)

(eval-when (:load-toplevel :compile-toplevel)
  (export '(tk-fileevent
	    iostream
	    read-fn
	    write-fn
	    eof-fn
	    mk-fileevent
	    stream-2-in-fd
	    stream-2-out-fd)))

;;; ===========================================================================
;;; TK-FILEEVENT MODEL
;;; ===========================================================================

(defmodel tk-fileevent (widget)
  
  ((.md-name
    :accessor id :initarg :id
    :initform (c-in nil)
    :documentation "ID of the fileevent instance.")

   (input-fd
    :accessor input-fd :initarg :input-fd
    :initform (c? (if (^iostream)
		      (stream-2-in-fd (^iostream))))
    :documentation "The input/read file descriptor - internal use only.")

   (output-fd
    :accessor output-fd
    :initarg :output-fd
    :initform (c? (if (^iostream)
	   	      (stream-2-out-fd (^iostream))))
    :documentation "The output/write file descriptor - internal use only.")

   (in-tcl-channel
    :accessor in-tcl-channel :initarg  :in-tcl-channel
    :initform (c? (fd-to-tcl-channel (^tki) (^input-fd)))
    :documentation "The TCL channel generated from the input file descriptor. - Internal use only.")

   (out-tcl-channel
    :accessor out-tcl-channel :initarg  :in-tcl-channel
    :initform (c? (fd-to-tcl-channel (^tki) (^output-fd)))
    :documentation "The TCL channel generated from the output file descriptor. - Internal use only.") 

   (in-tcl-ch-name
    :accessor in-tcl-ch-name :initarg  :in-tcl-ch-name
    :initform (c? (if (^in-tcl-channel)
	            (Tcl_GetChannelName (^in-tcl-channel))
	            nil))
    :documentation "The input TCL channel's name as passed to the fileevent command. - Internal use only.")

   (out-tcl-ch-name
    :accessor out-tcl-ch-name :initarg  :in-tcl-ch-name
    :initform (c? (if (^out-tcl-channel)
		    (Tcl_GetChannelName (^out-tcl-channel))
		    nil))
    :documentation "The output TCL channel's name as passed to the fileevent command. - Internal use only.") 

   (iostream
    :accessor iostream :initarg :iostream
    :initform (c-in nil)
    :documentation "The Lisp stream to be monitored - API: initarg,setf.")

   (readable-cb
    :accessor readable-cb :initarg :readable-cb
    :initform (c-in nil)
    :documentation "The readable callback. A dispatcher function used to call the function supplied via the read-fn slot. - Internal use only.")

   (writeable-cb
    :accessor writeable-cb :initarg :writeable-cb
    :initform (c-in nil)
    :documentation "The writeable callback. A dispatcher function used to call the function supplied via the read-fn slot. - Internal use only.")

   (eof-cb
    :accessor eof-cb :initarg :eof-cb
    :initform (c-in nil)
    :documentation "The eof callback. A dispatcher function used to call the function supplied via the eof-fn slot. - Internal use only.")

   (tki
    :accessor tki :initarg :tki
    :initform (c-in nil)
    :documentation "The Tcl/Tk Interpreter used. - API: initarg.")

   (opcode
    :accessor opcode :initarg :opcode
    :initform (file-event-opcode-cell-rule)
    :documentation "The opcode slot is used to control the operaion of the fileevent instance. - Internal use only.")

   (read-fn
    :accessor read-fn :initarg :read-fn
    :initform (c-in nil)
    :documentation "User supplied function, gets called when iostream is ready for reading. Gets iostream as parameter. - API: initarg, setf")

   (write-fn
    :accessor write-fn :initarg :write-fn
    :initform (c-in nil)
    :documentation "User supplied function, gets called when iostream is ready for writing. Gets iostream as parameter. - API: initarg, setf")

   (eof-fn
    :accessor eof-fn :initarg :eof-fn
    :initform (c-in nil)
    :documentation "User supplied function, gets called when iostream is EOF. Gets iostream as parameter. - API: initarg, setf (Via default-initarg set to fn default-eof-fn which simply closes the stream)."))

  (:default-initargs
      :id (gensym "tk-fileevent-")
    :eof-fn 'default-eof-fn))


;;; ===========================================================================
;;; CELL RULE: FILE-EVENT/OPCODE
;;; ===========================================================================
;;;
;;; Depending on opcode call the appropriate function to handle the various
;;; cases/combinations of input-fd, output-fd, and the previously executed
;;; update operation.

(defun file-event-opcode-cell-rule ()
  (c? ;; Set the opcode depending on values of input-fd, output-fd, iostream,
      ;; readable-cb, writeable-cb

      (if (and (not (^input-fd))
	       (not (^output-fd))
	       (not .cache))
        :nop
       
        (if (and (^input-fd)
		 (^iostream)
	         (^readable-cb))
	   :update-input-tk-fileevent
	   
	   (if (and (^output-fd)
		    (^iostream)
		    (^writeable-cb))
	       :update-output-tk-fileevent
	       
	      (if (and (not (^iostream))
		       (not (^input-fd)))
		 :reset-input-tk-fileevent
		 
		 (if (and (not (^iostream))
			  (not (^output-fd)))
		     :reset-output-tk-fileevent
		     :nop)))))))

;;; ===========================================================================
;;; INIT-TK-FILEEVENT - CALLED UPON INITIALIZATION
;;; ===========================================================================

(defun init-tk-fileevent (tki)
  (assert tki)
  ;; Nop - all init done in observers now.
)

;;; ===========================================================================
;;; FILEEVENT HELPER METHODS AND FUCTIONS
;;; ===========================================================================

(defmethod set-tk-readable ((self tk-fileevent) ch-name path)
  (tk-format-now "proc readable {channel path} { if [ eof $channel ] then { eof-cb $path } else { readable-cb $path } }")         
  (tk-format-now "fileevent ~A readable [list readable ~A ~A]"
		 ch-name
		 ch-name
		 path))

(defmethod set-tk-writeable ((self tk-fileevent) ch-name path)
  (tk-format-now "proc writeable {channel path} { if [ eof $channel ] then  { eof-cb $path } else { readable-cb $path } }")
  (tk-format-now "fileevent ~A writeable [list writeable ~A ~A]"
		 ch-name
		 ch-name
		 path))

;;; ===========================================================================
;;; OBSERVERS - USED TO SEND UPDATES TO TK LAND
;;; ===========================================================================

(defobserver opcode ((self tk-fileevent))
  (let ((*tki* (tki self)))
    (ecase new-value
    
      ((:init-tk-fileevent)
       (init-tk-fileevent (tki self)))
    
      ((:update-input-tk-fileevent)
       (let* ((channel (in-tcl-channel self))
	      (path    (path self))
	      (ch-name (Tcl_GetChannelName channel)))
	(set-tk-readable self ch-name path)))

      ((:update-output-tk-fileevent)
       (let* ((channel (out-tcl-channel self))
	      (path    (path self))
	      (ch-name (Tcl_GetChannelName channel)))
         (set-tk-writeable self ch-name path)))

      ((:reset-input-tk-fileevent)
       ;; Do nothing
       nil)

      ((:reset-output-tk-fileevent)
       ;; Do nothing
       nil)

      ((:nop)
       ;; Do nothing
       nil))))

(defobserver in-tcl-channel ((self tk-fileevent))
  (let ((*tki* (tki self)))
    (if (and new-value
	     (not old-value))
      (Tcl_RegisterChannel *tki* new-value))
    (if (and old-value (not new-value))
      (progn
	(tk-format-now "fileevent ~A readable {}"
		       (Tcl_GetChannelName old-value))
	(Tcl_UnregisterChannel *tki* old-value)))))

(defobserver out-tcl-channel ((self tk-fileevent))
  (let ((*tki* (tki self)))
    (if (and new-value (not old-value))
	 (Tcl_RegisterChannel *tki* new-value))
      (if (and old-value (not new-value))
	(progn
	  (tk-format-now "fileevent ~A writeable {}"
			 (Tcl_GetChannelName old-value))
	  (Tcl_UnregisterChannel *tki* old-value)))))

(defobserver readable-cb ((self tk-fileevent))
  (if new-value
    (Tcl_CreateCommand *tki*
		       "readable-cb"
		       new-value
		       (null-pointer)
		       (null-pointer))))

(defobserver writeable-cb ((self tk-fileevent))
  (if new-value
    (Tcl_CreateCommand *tki*
		       "writeable-cb"
		       new-value
		       (null-pointer)
		       (null-pointer))))

(defobserver eof-cb ((self tk-fileevent))
  (if new-value
    (Tcl_CreateCommand *tki*
		       "eof-cb"
		       new-value
		       (null-pointer)
		       (null-pointer))))

;;; ===========================================================================
;;; HELPER FUNCTIONS - FILE DESCRIPTOR TO STREAM AND CHANNEL
;;; ===========================================================================

(defun fd-to-tcl-channel (interp fd)
  (assert interp)
  (if fd
      (let ((channel (Tcl_MakeFileChannel fd 6))) ;; 6 = READ/WRITE
	(if channel
	    channel
	    (error "*** Tcl error: ~a" (tcl-get-string-result interp))))))


(defun stream-2-out-fd (stream) ;; FRGO: PORTING...

  #+allegro
    (excl:stream-output-fn stream)

  #-allegro
    (error "STREAM-2-OUT-FD: Not implemented for ~A Version ~A. Sorry."
	   (lisp-implementation-type)
	   (lisp-implementation-version))
)

(defun stream-2-in-fd (stream)  ;; FRGO: PORTING...
  
  #+allegro
    (excl:stream-input-fn stream)

  #-allegro
    (error "STREAM-2-IN-FD: Not implemented for ~A Version ~A. Sorry."
	   (lisp-implementation-type)
	   (lisp-implementation-version))
)

;;; ===========================================================================
;;; CALLBACKS
;;; ===========================================================================

(defcallback readable-cb :int
    ((clientData :pointer)
     (interp     :pointer)
     (argc       :int)
     (argv       :pointer))
  (declare (ignorable clientData argc interp))
  (let* ((path (foreign-string-to-lisp (mem-aref argv :pointer 1)))
	 (self (gethash path (dictionary *tkw*))))
    (bwhen (fn (^read-fn))
      (funcall fn self :read))) 
  (values (foreign-enum-value 'tcl-retcode-values :tcl-ok)))

(defcallback writeable-cb :int
    ((clientData :pointer)
     (interp     :pointer)
     (argc       :int)
     (argv       :pointer))
  (declare (ignorable clientData argc interp))
  (let* ((path (foreign-string-to-lisp (mem-aref argv :pointer 1)))
	 (self (gethash path (dictionary *tkw*))))
    (bwhen (fn (^write-fn))
      (funcall fn self :write)))
  (values (foreign-enum-value 'tcl-retcode-values :tcl-ok)))

(defcallback eof-cb :int
    ((clientData :pointer)
     (interp     :pointer)
     (argc       :int)
     (argv       :pointer))
  (declare (ignorable clientData interp argc))
  (let* ((path (foreign-string-to-lisp (mem-aref argv :pointer 1)))
	 (self (gethash path (dictionary *tkw*))))
    (bwhen (fn (^eof-fn))
	   (funcall fn self)))
  (values (foreign-enum-value 'tcl-retcode-values :tcl-ok)))

;;; ===========================================================================
;;; MK-FILEEVENT: CONVENIENCE MACRO
;;; ===========================================================================

(defmacro mk-fileevent (&rest inits)
  `(make-instance 'tk-fileevent
		  :tki *tki*
		  :readable-cb (get-callback 'readable-cb)
		  :writeable-cb (get-callback 'writeable-cb)
		  :eof-cb (get-callback 'eof-cb)
		  :fm-parent *parent*
		  ,@inits))

;;; ===========================================================================
;;; A DEFAULT EOF FUNCTION, USER MAY SUPPLY ANOTHER FUNCTION WHEN MAKING THE
;;; INSTANCE OF TK-FILEEVENT
;;; ===========================================================================

(defmethod default-eof-fn ((self tk-fileevent))
    ;; Default action: close stream
    (bwhen (iostream (^iostream))
      (close iostream)
      (setf (^iostream) nil)))

;;; ===========================================================================
;;; TESTING
;;; ===========================================================================
;;;
;;; With these few lines below we get a simple application with a text widget
;;; that shows data sent to a pipe in that text widget.
;;;
;;; The app does this by opening the named pipe for reading. It then waits
;;; for data on the pipe via the Tcl fileevent command. When establishing
;;; the fileevent a set of callbacks is established. The callbacks call
;;; two Lisp functions, depending on the type of channel (read or write.
;;;
;;; The callback functions look for the file channel's registered read or
;;; write functions. Those functions are set via the write-fn and read-fn
;;; methods of the tk-fileevent object.
;;;
;;; In the test example below we use the read case: the function read-from-pipe
;;; actually reads from the pipe and sends the data to the text widget by
;;; setting the text widgets model value.
;;; 
;;; In order to use this example please adapt the code below with a
;;; pipe name suitable for you (see the ^^^^^^^^ marks below).
;;; On Unixes you have to create the pipe with the mkfifo command.
;;;
;;; Have fun!
;;;
;;; Questions welcome...
;;;
;;; Frank Goenninger
;;; frgo@mac.com
;;;
;;; May 2006

(defmethod read-from-pipe ((self tk-fileevent) &optional (operation :read))
  (declare (ignorable operation))
  (let ((stream (^iostream)))
    (let ((data (read-line stream nil nil nil)))
      (trc "*** READ-FROM-PIPE: data = " data)
      (when data
        (setf (md-value (fm-other :receive-window)) data))))
)

(defmodel fileevent-test-window (window)
  ()
  (:default-initargs
      :kids (c? (the-kids
		   (mk-stack (:packing (c?pack-self))
 		     (mk-label :text "Receive window"
			       :pady 10)
		     (mk-text-widget :id :receive-window
				     ;:state 'disabled
				     :md-value (c-in "")
				     :height 10
				     :width 80
				     :borderwidth 2
				     :relief 'sunken
				     :pady 5))
		   (mk-fileevent :id :fileevent-test
				 :read-fn 'read-from-pipe
			         :iostream (open "/Users/frgo/tmp/frgo-test"
;;;                           Adapt here !!!     ^^^^^^^^^^^^^^^^^^^^^^^^^^^
					         :direction :input))))))

;;; Call this function  for testing !!
(defun test-fileevent ()
  (trc "-----------------------------------------------------------------------------")
  (test-window 'fileevent-test-window)
  (trc "-----------------------------------------------------------------------------"))