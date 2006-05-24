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

(in-package :Celtk)

;;;  --- packing ---------------------------------------------------------

(defobserver packing ()
  (when new-value
    (assert (null (kids-packing .parent)) ()
      "Do not specify packing (here for ~a) unless parent leaves kids-packing unspecified. 
This parent is ~a, kids-packing ~a" self (list .parent (type-of .parent)) (kids-packing .parent)))
  ;
  ; This use next of the parent instead of self is pretty tricky. It has to do with getting
  ; the pack commands out nested widgets before parents. The pack command issued on behalf
  ; of a top frame is sorted on the parent. Now we have to pack the top frame. If we associate
  ; the command with the frame, the sort is a tie and either might go first. So we continue
  ; the theme and associate /this/ pack with this top frame's parent. Note that we cannot go the
  ; normal route and pack the kids in their own context, because multiple kids get packed
  ; in one pack statement (and we cannot arbitrarily pack with the first kid because this is a nested
  ; deal and any kid might have kids, so each family packs associated with itself)
  ;
  (when (and new-value (not (typep .parent 'panedwindow)))
    (tk-format `(:pack ,(fm-parent self)) new-value)))

(defmacro c?pack-self (&optional (modifier$ ""))
  `(c? (format nil "pack ~a ~a" (path self) ,modifier$)))

;;; --- grids -------------------------------------------------------------------------

(defmodel grid-manager ()())

(defobserver gridding ((self grid-manager))
  (when new-value
    (loop for k in (^kids)
          when (gridding k)
          do (tk-format `(:grid ,k) (format nil "grid ~a ~a" (path k) (gridding k))))
    (destructuring-bind (&key columns rows) new-value
      (when columns
        (loop for config in columns
              for idx upfrom 0
              do (tk-format `(:grid ,self) (format nil "grid columnconfigure ~a ~a ~a" (^path) idx config))))
      (when columns
        (loop for config in rows
              for idx upfrom 0
              do (tk-format `(:grid ,self) (format nil "grid rowconfigure ~a ~a ~a" (^path) idx config)))))))

;;; --- Layout ------------

(eval-when (compile load eval)
  (export '( b-left b-top b-right b-bottom b-width b-height
             l-bounds l-left l-top l-right l-left l-top l-right l-bottom l-width l-height
             p-offset ^p-offset p-bounds ^p-bounds p-left p-top p-right p-bottom
             make-bounds p-center-vt b-center-vt p-center-hz
             c-offset c-bounds offset+)))

(defun bounds-offset (b x-y)
  (destructuring-bind (x y) x-y
    (vector (+ (svref b 0) x)
      (+ (svref b 1) y)
      (+ (svref b 2) x)
      (+ (svref b 3) y))))

(defun c-offset (self)
  (assert (typep self 'item-geometer)() "~a is not typep item-geomete. Type is ~a" self (type-of self))
  (if (or (null .parent) (typep .parent 'canvas))
      (eko (nil "c-offset at top" self (type-of self) .parent)
        (progn
          (unless .parent (break "no parent for ~a?!" self))
          #+not (when (and (null .parent)(typep self 'mathx::mx-theq))
            (break))
          (^p-offset)))
    (offset+ (p-offset self) (c-offset .parent))))

(defun c-bounds (self) ;; make this a slot?
  (assert (typep self 'item))
  (bounds-offset (l-bounds self) (c-offset self)))

(defmacro b-left (b) `(svref ,b 0))
(defmacro b-top (b) `(svref ,b 1))
(defmacro b-right (b) `(svref ,b 2))
(defmacro b-bottom (b) `(svref ,b 3))
(defun b-width (b) (- (b-right b) (b-left b)))
(defun b-height (b) (- (b-bottom b) (b-top b)))

(defmacro l-left (mx) `(b-left (l-bounds ,mx)))
(defmacro l-top (mx) `(b-top (l-bounds ,mx)))
(defmacro l-right (mx) `(b-right (l-bounds ,mx)))
(defmacro l-bottom (mx) `(b-bottom (l-bounds ,mx)))
(defun l-center-vt (self)
  (floor (+ (l-top self)(l-bottom self)) 2))

(defun l-width (mx) (b-width (l-bounds mx)))
(defun l-height (mx) (b-height (l-bounds mx)))

(defmacro p-left (mx) `(b-left (p-bounds ,mx)))
(defmacro p-top (mx) `(b-top (p-bounds ,mx)))
(defmacro p-right (mx) `(b-right (p-bounds ,mx)))
(defmacro p-bottom (mx) `(b-bottom (p-bounds ,mx)))

(defun make-bounds (left top right bottom)
  (vector left top right bottom))

(defun p-center-vt (self)
  (b-center-vt (p-bounds self)))

(defun b-center-vt (b)
  (floor (+ (b-bottom b)(b-top b)) 2))

(defun p-center-hz (self)
  (b-center-hz (p-bounds self)))

(defun b-center-hz (b)
  (floor (+ (b-left b)(b-right b)) 2))

(defun offset+ (off1 off2)
  (mapcar '+ off1 off2))






