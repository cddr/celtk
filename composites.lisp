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



;;; --- toplevel ---------------------------------------------

(deftk toplevel (widget)
  ()
  (:tk-spec toplevel
    -borderwidth -cursor -highlightbackground -highlightcolor
    -highlightthickness -padx -pady -relief
    -takefocus -background -tk-class -colormap
    -container -height -menu -screen
    -use -visual -width)
  (:default-initargs
      :id (gentemp "TOP")))

;; --- panedwindow -----------------------------------------

(deftk panedwindow (widget)
  ()
  (:tk-spec panedwindow
    -background -borderwidth -cursor -height
    -orient -relief -width
    -handlepad
    -handlesize
    -opaqueresize
    -sashcursor
    -sashpad
    -sashrelief
    -sashwidth
    -showhandle)
  (:default-initargs
      :id (gentemp "PW")
      :packing nil))

(defmethod make-tk-instance ((self panedwindow))
  (tk-format `(:make-tk ,self) "panedwindow ~a -orient ~(~a~)"
    (^path) (or (orient self) "vertical"))
  (tk-format `(:pack ,self) "pack ~a -expand yes -fill both" (^path)))

(defmethod parent-path ((self panedwindow)) (^path))

(defobserver .kids ((self panedwindow))
  (loop for k in (^kids)
      do (trc "panedwindow adds" k (type-of k) (md-name k) (path k))
        (tk-format `(:post-make-tk ,self) "~a add ~a" (^path) (path k))))

; --------------------------------------------------------

(defmodel composite-widget (widget)
  ((kids-packing :initarg :kids-packing :accessor kids-packing :initform nil)))

(eval-when (compile load eval)
  (export '(title$ active)))

(defmodel window (composite-widget)
  (#+wishful (wish :initarg :wish :accessor wish
               :initform (wish-stream *wish*)
               #+(or) (c? (do-execute "wish85 -name testwindow" 
                            nil #+not (list (format nil "-name ~s" (title$ self))))))
    #+wishful (ewish :initarg :ewish :accessor ewish :initform nil :cell nil) ;; vestigial?
    (title$ :initarg :title$ :accessor title$
      :initform (c? (string-capitalize (class-name (class-of self)))))
    (dictionary :initarg :dictionary :initform (make-hash-table :test 'string-equal) :accessor dictionary)
    (callbacks :initarg :callbacks :accessor callbacks
      :initform (make-hash-table :test #'eq))
    (after-timers :initarg :after-timers :accessor after-timers :initform (make-hash-table))
    (edit-style :initarg :edit-style :accessor edit-style :initform (c-in nil))
    (tk-scaling :initarg :tk-scaling :accessor tk-scaling
      :initform (c? 1.3 #+tki (read-from-string (tk-eval "tk scaling"))))
    (tkfonts-to-load :initarg :tkfonts-to-load :accessor tkfonts-to-load :initform nil)
    (tkfont-sizes-to-load :initarg :tkfont-sizes-to-load :accessor tkfont-sizes-to-load :initform nil)
    (tkfont-info :initarg :tkfont-info :accessor tkfont-info
      :initform (tkfont-info-loader))
    (initial-focus :initarg :initial-focus :accessor initial-focus :initform nil))
  )

(defobserver initial-focus ()
  (when new-value
    (tk-format '(:fini new-value) "focus ~a" (path new-value))))

(defun tkfont-info-loader ()
    (c? (eko (nil "tkfinfo")
          (loop with scaling = (^tk-scaling)
              for (tkfont fname) in (^tkfonts-to-load)
              collect (cons tkfont
                        (apply 'vector
                          (loop for fsize in (^tkfont-sizes-to-load)
                                for id = (format nil "~(~a-~2,'0d~)" tkfont fsize)
                              for tkf = (tk-eval "font create ~a -family {~a} -size ~a"
                                          id fname fsize)
                              for (nil ascent nil descent nil linespace nil fixed) = (tk-eval-list "font metrics ~a" tkf)
                              collect (make-tkfinfo :ascent (round (parse-integer ascent) scaling)
                                        :id id
                                        :family fname
                                        :size fsize
                                        :descent (round (parse-integer descent) scaling)
                                        :linespace (round (parse-integer linespace) scaling)
                                        :fixed (plusp (parse-integer fixed))
                                        :em (round (parse-integer
                                                    (tk-eval "font measure ~(~a~) \"m\"" tkfont))
                                              scaling)))))))))

(defobserver title$ ((self window))
   (tk-format '(:configure "title") "wm title . ~s" (or new-value "Untitled")))

(defmethod path ((self window)) ".")
(defmethod parent-path ((self window)) "")

