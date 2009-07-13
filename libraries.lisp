;; -*- mode: Lisp; Syntax: Common-Lisp; Package: celtk; -*-
#|

    Celtk -- Cells, Tcl, and Tk

Copyright (C) 2006 by Andy Chambers

This library is free software; you can redistribute it and/or
modify it under the terms of the Lisp Lesser GNU Public License
 (http://opensource.franz.com/preamble.html), known as the LLGPL.

This library is distributed  WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

See the Lisp Lesser GNU Public License for more details.

|#

(in-package :celtk)

;; Tcl/Tk
;;
;; Post v8.5, tk comes bundled with tile so we no longer
;; need to define a foreign library for it.

(define-foreign-library tcl
    (:darwin (:framework "Tcl"))
  (:windows (:or "Tcl85.dll"))
  (:unix (:or "libtcl8.5.so" "libtcl.so"))
  (t (:default "libtcl")))

(use-foreign-library tcl)

(define-foreign-library Tk
    (:darwin (:framework "Tk"))
  (:windows (:or "Tk85.dll"))
  (:unix (:or "libtk8.5.so" "libtk.so"))
  (t (:default "libtk")))

(use-foreign-library tk)

