;; -*- mode: Lisp; Syntax: Common-Lisp; Package: cells; -*-
#|

    QuicktimeTcl Interfaces

Copyright (C) 2007 by Kenneth Tilton

This library is free software; you can redistribute it and/or
modify it under the terms of the Lisp Lesser GNU Public License
 (http://opensource.franz.com/preamble.html), known as the LLGPL.

This library is distributed  WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

See the Lisp Lesser GNU Public License for more details.

|#

(in-package :celtk)

(export! mk-movie url tk-file)
(deftk movie (widget)
  ()
  (:tk-spec movie -url (tk-file -file))
  (:default-initargs
      :tile? nil))

(defobserver tk-file :around ((self movie))
  (call-next-method)
  (when (and new-value old-value)
    (tk-format `(:fini ,self) "~a play" (^path))))
