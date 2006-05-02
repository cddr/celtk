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

(in-package :Celtk)

;;; --- tk-object ------------------


(defmodel tk-object (model)
  ((.md-name :cell nil :initform (gentemp "TK") :initarg :id)
   (tk-class :cell nil :initform nil :initarg :tk-class :reader tk-class)
   
   (timers :initarg :timers :accessor timers :initform nil)
   (on-command :initarg :on-command :accessor on-command :initform nil)
   (user-errors :initarg :user-errors :accessor user-errors :initform nil))
  (:documentation "Root class for widgets and (canvas) items"))

(defmethod md-awaken :before ((self tk-object))
  (with-integrity (:client `(:make-tk ,self))
    (make-tk-instance self)))

;;; --- deftk --------------------

(defmacro deftk (class superclasses
                         (&rest std-slots)
                         &rest defclass-options)
  (destructuring-bind (&optional tk-class &rest tk-options)
      (cdr (find :tk-spec defclass-options :key 'car))
    
    (setf tk-options (tk-options-normalize tk-options))
    
    (multiple-value-bind (slots outputs)
        (loop for (slot-name tk-option) in tk-options
            collecting `(,slot-name :initform nil
                          :initarg ,(intern (string slot-name) :keyword)
                          :accessor ,slot-name)
            into slot-defs
            when tk-option
            collecting `(setf (get ',slot-name 'tk-config-option) ',tk-option)
            into outputs
            finally (return (values slot-defs outputs)))
      `(eval-when (compile load eval)
         (defmodel ,class ,(or superclasses '(tk-object))
           (,@(append std-slots slots))
           ,@(remove-if (lambda (k) (find k '(:default-initargs :tk-spec))) defclass-options :key 'car)
           (:default-initargs
               ,@(when tk-class `(:tk-class ',tk-class))
             ,@(cdr (find :default-initargs defclass-options :key 'car))))
         (defmethod tk-class-options append ((self ,class))
           ',tk-options)
         (export ',(loop for (slot nil) in tk-options
                        nconcing (list slot (intern (conc$ "^" slot)))))
         (defmacro ,(intern (conc$ "MK-" (symbol-name class))) (&rest inits)
           `(make-instance ',',class
              :fm-parent *parent*
              ,@inits))
         ,@outputs))))

(defun tk-options-normalize (tk-options)
  "normalize '(-aaa (tk-bbb -bbb)) => '((aaa -aaa)(tk-bbb -bbb))"
  (loop for tk-option-def in tk-options
      for slot-name = (intern (de- (if (atom tk-option-def)
                                       tk-option-def (car tk-option-def))))
      collecting (list slot-name (if (atom tk-option-def)
                                     tk-option-def (cadr tk-option-def)))))

(eval-when (compile load eval)
  (defun de- (sym)
    (remove #\- (symbol-name sym) :end 1)))
  
(defgeneric tk-class-options (self)
  (:method-combination append))

(defmethod slot-value-observe progn (slot-name (self tk-object) new-value old-value old-value-boundp)
  (declare (ignorable old-value))
  (when old-value-boundp ;; initial propagation to Tk happens during make-tk-instance
    (bwhen (tk-config-option (get slot-name 'tk-config-option))
      (tk-configure self (string tk-config-option) (or new-value "")))))

(defun tk-configurations (self)
  (loop with configs
      for (slot-name tk-option) in (or (get (type-of self) 'tk-class-options)
                                     (setf (get (type-of self) 'tk-class-options)
                                       (remove-duplicates (tk-class-options self) :key 'second)))
      when tk-option
      do (bwhen (slot-value (funcall slot-name self)) ;; must go thru accessor with Cells, not 'slot-value
           (setf configs (nconc (list tk-option (tk-send-value slot-value)) configs)))
      finally (return configs)))

