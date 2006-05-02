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

;;; --- fonts ---------------

(eval-when (compile load eval)
  (export '(make-finfo finfo-family finfo-size finfo-slant finfo-ascent  finfo-linespace finfo-fixed
             font-id font-info bounds-offset finfo-ascent font-height font-ascent 
             finfo-descent ^font-descent ^font-find
             finfo finfo-em ^font-em 
             line-up line-down font-size-info)))

(defmacro def^macros (&rest fn-names)
  `(progn ,@(loop for fn-name in fn-names
                  collecting (let ((^name (format nil "^~:@(~a~)" fn-name)))
                               `(progn
                                  (eval-when (compile load eval)
                                    (export '(,(intern ^name))))
                                  (defmacro ,(intern ^name) ()
                                    `(,',fn-name self)))))))

(def^macros line-up line-down font-height font-ascent finfo-descent)

(defstruct finfo id family size slant ascent descent linespace fixed em)

(deftk font (widget)
  ()
  (:tk-spec font
    -family -size -weight -slant -underline -overstrike)
  (:default-initargs
      :id (gentemp "fnt")))

(defmethod make-tk-instance ((self font))
  (setf (gethash (^path) (dictionary .tkw)) self)
  (tk-format-now "font create ~a ~{~(~a~) ~a~^ ~}"
      (font-id self)(tk-configurations self)))

(defmethod tk-configure ((self font) option value)
  (tk-format `(:configure ,self ,option) "font configure ~(~a~) ~(~a~) ~a"
    (path self) option (tk-send-value value)))

(defun font-id (font) (md-name font))

(defmethod path ((self font))
  (font-id self))

(defmacro ^font-find (font-id)
  `(cdr (assoc ,font-id (font-info .tkw))))
      
(defmodel fontified ()
  ((fkey :initarg :fkey :accessor fkey :initform nil)
   (f-size-step :initarg :f-size-step :accessor f-size-step
     :initform 0)
   (finfo :initarg :finfo :accessor finfo
     :initform (c_? (bwhen (fkey (^fkey))
                       (let ((fkey-table (cdr (assoc fkey (font-info .tkw)))))
                         (ASSERT fkey-table () "no such font: ~a ~a" fkey (symbol-package fkey))
                         (svref fkey-table (^f-size-step)))))))
  (:default-initargs
      :font (c_? (bwhen (fi (^finfo))
                  (finfo-id fi)))))

(defun font-size-info (self font decrements)
  (let ((font-size-table (cdr (assoc font (font-info .tkw)))))
    (ASSERT font-size-table () "no such font: ~a ~a" font (symbol-package font))
    (svref font-size-table (+ 2 decrements)))) ;; we allow -decrements as a guess that it will be needed. dumb. :)

(defun font-ascent (self)
  (finfo-ascent (^finfo)))

(defun font-height (self)
  (finfo-linespace (^finfo)))

(defun line-up (self)
  (ceiling (font-height self) -2))

(defun line-down (self)
  (floor (font-height self) 2))



