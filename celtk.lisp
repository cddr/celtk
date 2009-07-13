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

(defvar *tki* nil
  "The tcl/tk interpreter stream")

(defparameter *windows-being-destroyed* nil
  "When destroying a widget, add it to this list to ensure
no attempt is made to destroy it when destroying children of
that widget")

(defparameter *windows-destroyed* nil)

(defparameter *tk-last* nil
  "Debug aid. Last recorded command send to Tk")

(defparameter *tkw* nil
  "The root tk window")

(define-symbol-macro .tkw (nearest self window))

; --- tk-format --- talking to wish/Tk -----------------------------------------

(defparameter +tk-client-task-priority+
  '(
    :delete
    :forget
    :destroy
    :pre-make-tk
    :make-tk
    :make-tk-menubutton
    :post-make-tk
    :variable
    :bind
    :selection
    :trace
    :configure
    :grid
    :pack
    :fini
    ))

(defun defer-info-p (info)
  (find (car info)
	+tk-client-task-priority+))
(deftype defer-info ()
  `(or (eql :grouped)
    (and list
     (satisfies defer-info-p))))


(defun tk-user-queue-sort (task1 task2)
  "Intended for use as user queue sorter, to make Tk happy by giving it
stuff in the order it needs to work properly."
  (destructuring-bind (type1 self1 &rest dbg) task1
      (declare (ignorable dbg))
      (destructuring-bind (type2 self2 &rest dbg) task2
        (declare (ignorable dbg))
        (let ((p1 (position type1 +tk-client-task-priority+))
              (p2 (position type2 +tk-client-task-priority+)))
          (cond
           ((< p1 p2) t)
           ((< p2 p1) nil)
           (t (case type1 ;; they are the same if we are here
                (:make-tk
                 (fm-ordered-p self1 self2))
                (:pack
                 (fm-ascendant-p self2 self1)))))))))

(defun tk-user-queue-handler (user-q)
  (labels ((validate-queue (q)
	     (loop for (info . nil) in (fifo-data q)
		unless (typep info 'defer-info)
		do (error "unknown tk client task type ~a in task: ~a "
			  (car info)
			  info)))
	   (sort-queue (q)
	     (stable-sort (fifo-data q)
			  'tk-user-queue-sort
			  :key 'car))
	   (process-queue (q)
	     (loop for (defer-info . task) in (prog1 (sort-queue q)
						(fifo-clear q))
		do
		  (trc nil "!!! --- tk-user-queue-handler dispatching" defer-info)
		  (funcall task :user-q defer-info))))

    (validate-queue user-q)
    (process-queue user-q)))



(defun tk-format-now (fmt$ &rest fmt-args)
  (unless (find *tkw* *windows-destroyed*)
    (let* ((*print-circle* nil)
           (tk$ (apply 'format nil fmt$ fmt-args)))
      (let ((yes ) ; '("menubar" "cd"))
            (no  '()))
        (declare (ignorable yes no))
        (when (find-if (lambda (s) (search s tk$)) yes)
          (format t "~&tk> ~a~%" tk$)))
      (assert *tki*)
      (setf *tk-last* tk$)
      (tcl-eval-ex *tki* tk$))))


(defun tk-format (defer-info fmt$ &rest fmt-args)
  "Format then send to wish (via user queue)"
  (assert (typep defer-info 'defer-info)
	  () (conc$ "Need defer-info to sort command ~a."
		    "Specify :grouped if caller is managing user-queue")
	  (apply 'format nil fmt$ fmt-args))

  (flet ((do-it ()
           (apply 'tk-format-now fmt$ fmt-args)))
    (if (eq defer-info :grouped)
	(do-it)
        (with-integrity (:client defer-info)
          (do-it)))))


(defmethod tk-send-value ((s string))
  (if (find #\space s)
      (format nil "{~a}" s)
    (format nil "~s" s)))

(defmethod tk-send-value (other)
  (format nil "~a" other))

(defmethod tk-send-value ((s symbol))
  (down$ s))

(defmethod tk-send-value ((p package))
  (package-name p))

(defmethod tk-send-value ((values list))
  (format nil "{~{~a~^ ~}}" (mapcar 'tk-send-value values)))

(defmethod parent-path ((nada null)) "")
(defmethod parent-path ((other t)) "")


; --- tk eval  ----------------------------------------------------

(defmethod path-index (self) (path self))

(defun tk-eval (tk-form$ &rest fmt-args
                 &aux (tk$ (apply 'format nil tk-form$ fmt-args)))
  (assert *tki* () (conc$ "Global *tki* is not bound to anything, "
			  "let alone a Tcl interpreter"))
  (tk-format :grouped tk$)
  (tcl-get-string-result *tki*)
  )

(defun tk-eval-var (var)
  (tk-eval "set ~a" var))

(defun tk-eval-list (tk-form$ &rest fmt-args)
  (tk-format :grouped (apply 'format nil tk-form$ fmt-args))
  (parse-tcl-list-result (tcl-get-string-result *tki*)))

#+test
(parse-tcl-list-result "-ascent 58 -descent 15 -linespace 73 -fixed 0")

(defun parse-tcl-list-result (result &aux item items)
  (when (plusp (length result))
    (trc nil "parse-tcl-list-result" result)
    (labels ((is-spaces (s)
               (every (lambda (c) (eql c #\space)) s))
             (gather-item ()
               (unless (is-spaces item)
                 ;(trc "item chars" (reverse item))
                 ;(trc "item string" (coerce (reverse item) 'string))
                 (push (coerce (nreverse item) 'string) items)
                 (setf item nil))))
      (loop with inside-braces
          for ch across result
          if (eql ch #\{)
          do (if inside-braces
                 (break "whoa, nested braces: ~a" result)
               (setf inside-braces t))
          else if (eql ch #\})
          do (setf inside-braces nil)
            (gather-item)
            (setf item nil)
          else if (eql ch #\space)
          if inside-braces do (push ch item)
          else do (gather-item)
            (setf item nil)
          else do (push ch item)
          finally (gather-item)
            (return (nreverse items))))))







