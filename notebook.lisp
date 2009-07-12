;; -*- mode: Lisp; Syntax: Common-Lisp; Package: cells; -*-
#|

    Celtk -- Cells, Tcl, and Tk

Copyright (C) 2009 by Andy Chambers

This library is free software; you can redistribute it and/or
modify it under the terms of the Lisp Lesser GNU Public License
 (http://opensource.franz.com/preamble.html), known as the LLGPL.

This library is distributed  WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

See the Lisp Lesser GNU Public License for more details.

|#

(in-package :celtk)

;--- n o t e b o o k ----------------------------------------------

(deftk notebook (widget decoration-mixin)
 ()
 (:tk-spec notebook
   -height -padding -width)
 (:default-initargs
     :id (gentemp "NB")
   :packing nil))

(defmethod make-tk-instance ((self notebook))
 (tk-format `(:make-tk ,self) "ttk::notebook ~a" (^path))
 (tk-format `(:pack ,self) "pack ~a -expand yes -fill both" (^path)))

(defobserver .kids ((self notebook))
 (loop for k in (^kids)
     do (trc "ttk::notebook adds" k (type-of k) (md-name k) (path k))
       (tk-format `(:post-make-tk ,self) "~a add ~a -text ~a"
                                         (^path)
                                         (path k)
                                         (text k))))

;--- t a b -----------------------------------------------------------

(deftk tab (frame-stack widget)
 ()
 (:tk-spec tab
   -state -sticky -padding -text -image)
 (:default-initargs
     :id (gentemp "TB")))


(defmacro mk-tab ((&rest inits) &body body)
 `(make-instance 'tab :fm-parent *parent* ,@inits
                 :kids (c? (the-kids
                            ,@body))))

(defmethod make-tk-instance ((self tab))
 (tk-format `(:make-tk ,self) "frame ~a" (^path)))

;--- example usage ---------------------------------------------------

(defmd nb-test (window)
 (kids (c? (the-kids
            (mk-notebook
             :width 100
             :kids (c? (the-kids
                        (mk-tab (:text "first")
                          (mk-stack ("tab with container")
                            (mk-label :text "hi")))
                        (mk-tab (:text "second")
                          (mk-label :text "a")
                          (mk-label :text "b")))))))))

(defun test-nb ()
 (test-window 'nb-test))

