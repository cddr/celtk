;; -*- mode: Lisp; Syntax: Common-Lisp; Package: celtk; -*-
#|

    Celtk -- Cells, Tcl, and Tk

Copyright (C) 2006 by Kenneth Tilton
Copyright (C) 2009 by Andy Chambers

This library is free software; you can redistribute it and/or
modify it under the terms of the Lisp Lesser GNU Public License
 (http://opensource.franz.com/preamble.html), known as the LLGPL.

This library is distributed  WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

See the Lisp Lesser GNU Public License for more details.

|#

(in-package :celtk-tests)

(defun run-tests ()
  (run! 'celtk-test-suite))

(run 'celtk-test-suite)
