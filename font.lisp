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

;;; --- fonts obtained from Tk-land ---------------

(eval-when (compile load eval)
  (export '(make-tkfinfo tkfinfo-family tkfinfo-size tkfinfo-slant tkfinfo-ascent  tkfinfo-linespace tkfinfo-fixed
             tkfont-id tkfont-info bounds-offset tkfinfo-ascent tkfont-height tkfont-ascent 
             tkfinfo-descent ^tkfont-descent ^tkfont-find
             tkfinfo tkfinfo-em ^tkfont-em 
             line-up line-down tkfont-size-info)))

(defmacro def^macros (&rest fn-names)
  `(progn ,@(loop for fn-name in fn-names
                  collecting (let ((^name (format nil "^~:@(~a~)" fn-name)))
                               `(progn
                                  (eval-when (compile load eval)
                                    (export '(,(intern ^name))))
                                  (defmacro ,(intern ^name) ()
                                    `(,',fn-name self)))))))

(def^macros line-up line-down tkfont-height tkfont-ascent tkfinfo-descent)

(defstruct tkfinfo id family size slant ascent descent linespace fixed em)

(deftk tkfont (widget)
  ()
  (:tk-spec font
    -family -size -weight -slant -underline -overstrike)
  (:default-initargs
      :id (gentemp "fnt")))

(defmethod make-tk-instance ((self tkfont))
  (setf (gethash (^path) (dictionary .tkw)) self)
  (tk-format `(:make-tk ,self) "font create ~a ~{~(~a~) ~a~^ ~}"
      (tkfont-id self)(tk-configurations self)))

(defmethod tk-configure ((self tkfont) option value)
  (tk-format `(:configure ,self ,option) "font configure ~(~a~) ~(~a~) ~a"
    (path self) option (tk-send-value value)))

(defun tkfont-id (tkfont) (md-name tkfont))

(defmethod path ((self tkfont))
  (tkfont-id self))

(defmacro ^tkfont-find (tkfont-id)
  `(cdr (assoc ,tkfont-id (tkfont-info .tkw))))
      
(defmodel tkfontified ()
  ((fkey :initarg :fkey :accessor fkey :initform nil)
   (f-size-step :initarg :f-size-step :accessor f-size-step
     :initform 0)
   (tkfinfo :initarg :tkfinfo :accessor tkfinfo
     :initform (c_? (bwhen (fkey (^fkey))
                       (let ((fkey-table (cdr (assoc fkey (tkfont-info .tkw)))))
                         (ASSERT fkey-table () "no such tkfont: ~a ~a" fkey (symbol-package fkey))
                         (svref fkey-table (^f-size-step)))))))
  (:default-initargs
      :tkfont (c_? (bwhen (fi (^tkfinfo))
                  (tkfinfo-id fi)))))

(defun tkfont-size-info (self tkfont decrements)
  (let ((tkfont-size-table (cdr (assoc tkfont (tkfont-info .tkw)))))
    (ASSERT tkfont-size-table () "no such tkfont: ~a ~a" tkfont (symbol-package tkfont))
    (svref tkfont-size-table (+ 2 decrements)))) ;; we allow -decrements as a guess that it will be needed. dumb. :)

(defun tkfont-ascent (self)
  (tkfinfo-ascent (^tkfinfo)))

(defun tkfont-height (self)
  (tkfinfo-linespace (^tkfinfo)))

(defun line-up (self)
  (ceiling (tkfont-height self) -2))

(defun line-down (self)
  (floor (tkfont-height self) 2))



