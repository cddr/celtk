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

(deftk label (widget)
  ()
  (:tk-spec label
    -activebackground   -activeforeground  -anchor
    -background -bitmap -borderwidth -compound -cursor
    -disabledforeground   (tkfont -font) -foreground
    -highlightbackground -highlightcolor -highlightthickness -image
    (tk-justify -justify)
    -padx -pady -relief -takefocus -text -textvariable -underline
    -height -state -width -wraplength)
  (:default-initargs
      :id (gentemp "LBL")))

;--------------------------------------------------------------------------

(deftk message (widget)
  ()
  (:tk-spec message
    -activebackground   -activeforeground  -anchor
    -background -bitmap -borderwidth -compound -cursor
    -disabledforeground    (tkfont -font) -foreground
    -highlightbackground -highlightcolor -highlightthickness -image
    (tk-justify -justify) 
    -padx -pady -relief 
    -takefocus -text -textvariable
    -underline -wraplength -width -state -height)
  (:default-initargs
      :id (gentemp "MSG")))

