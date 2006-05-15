;; -*- mode: Lisp; Syntax: Common-Lisp; Package: celtk; -*-
;;;
;;; Copyright (c) 2006 by Kenneth William Tilton.
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy 
;;; of this software and associated documentation files (the "Software"), to deal 
;;; in the Software without restriction, including without limitation the rights 
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell 
;;; copies of the Software, and to permit persons to whom the Software is furnished 
;;; to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in 
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS 
;;; IN THE SOFTWARE.

;;; --- timers ----------------------------------------

(in-package :Celtk)

(defun never-unchanged (new old) (declare (ignore new old)))

;;;
;;; Now, not one but three incredibly hairy gyrations Cells-wise:
;;;
;;;    - repeat cannot be ephemeral, but we want repeated (setf (^repeat) 20)'s each to fire,
;;;      so we specify an unchanged-if value that always "no", lying to get propagation
;;;
;;;    - the executions rule is true obfuscated code. It manages to reset the count to zero
;;;      on repeated (setf ... 20)'s because on the second repetition we know we will hit the rule
;;;      with repeat non-null (20, in fact) and the ephemeral executed will be nil (because it is
;;;      only non-nil during propagation of (setf (executed...) t). not for Cell noobs.
;;;
;;;    - holy toledo. The /rule/ for after-factory sends the after command to Tk itself! I could just
;;;      return a list of the delay and the callback and have an observer dispatch it, but it would
;;;      have to so so exactly as the rule does, by dropping it in the deferred client queue.
;;;      In a sense I am starting here to leverage Cells3 queues to simplify things. Mind you, if
;;;      Timer evolves to where we let the client write its own after factory, we might want to
;;;      factor out the actual dispatch into an observer to make it transparent (assuming that is
;;;      not why they are supplying their own after-factory.
;;;
;;; Timer is totally a work-in-progress with much development ahead.
;;;

(eval-when (compile load eval)
  (export '(repeat ^repeat)))

(defmodel timer ()
  ((id :cell nil :initarg :id :accessor id :initform (gentemp "AFTER")
     :documentation "A debugging aid")
   (tag :cell nil :initarg :tag :accessor tag :initform :anon
     :documentation "A debugging aid")
   (elapsed :cell nil :initarg :elapsed :accessor elapsed :initform 0
     :documentation "A debugging aid")
   (state :initarg :state :accessor state :initform (c-in :on)
     :documentation "Turn off to stop, regardless of REPEAT setting") ;; possibly redundant
   (action :initform nil :initarg :action :accessor action
     :documentation "A function invoked when the TCL AFTER executes (is dispatched)")
   (delay :initform 0 :initarg :delay :accessor delay
     :documentation "Millisecond interval supplied as is to TCL AFTER")
   (repeat :initform (c-in nil) :initarg :repeat :accessor repeat :unchanged-if 'never-unchanged
     :documentation "t = run continuously, nil = pause, a number N = repeat N times")
   (executed :cell :ephemeral :initarg :executed :accessor executed :initform (c-in nil)
     :documentation "Internal boolean: set after an execution")
   (executions :initarg :executions :accessor executions
     :documentation "Number of times timer has had its action run since the last change to  the repeat slot"
     :initform (c? (eko (nil ">>> executions")
                     (if (null (^repeat))
                       0 ;; ok, repeat is off, safe to reset the counter here
                     (if (^executed)
                         (1+ (or .cache 0)) ;; obviously (.cache is the prior value, and playing it safe in case unset)
                       0))))) ;; hunh? executed is ephemeral. we are here only if repeat is changed, so reset
   
   (on-command :reader on-command
     :initform (lambda (self)
                 (when (eq (^state) :on)
                     (assert (^action))
                     (funcall (^action) self)
                     (setf (^executed) t))))
   (after-factory :reader after-factory
     :initform (c? (bwhen (rpt (eko (nil ">>> repeat") (when (eq (^state) :on)
                               (^repeat))))
                   (when (or (zerop (^executions)) (^executed)) ;; dispatch initially or after an execution
                     (if (zerop (^executions))
                         (setf (elapsed self) (now))
                       (when (and (numberp rpt)
                               (>= (^executions) rpt))
                         (print `(stop timer!!! ,(* 1.0 (- (now) (elapsed self)))))))
                     (when (if (numberp rpt)
                               (< (^executions) rpt)
                             rpt) ;; playing it safe/robust: redundant with initial bwhen check that rpt is not nil
                       (with-integrity (:client `(:fini ,self)) ;; just guessing as to when, not sure it matters
                         (setf (id self) (set-timer self (^delay)))))))))))

(defun set-timer (self time)
  (setf (gethash (id self) (dictionary *tkw*)) self) ;; redundant but fast
  (tk-eval "after ~a {event generate . <<time-is-up>> -data ~a}" time (id self)))

(defobserver timers ((self tk-object) new-value old-value)
  (dolist (k (set-difference old-value new-value))
    (setf (state k) :off)
    (when (id k)
      (tk-format-now "after cancel ~a" (id k))))) ;; Tk doc says OK if cancelling already executed


    