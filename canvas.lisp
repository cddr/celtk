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

(deftk canvas ()
  ()
  (:tk-spec canvas
    -background -borderwidth -cursor
    -highlightbackground -highlightcolor -highlightthickness
    -insertbackground -insertborderwidth -insertofftime -insertontime -insertwidth
    -relief -selectbackground -selectborderwidth -selectforeground
    -state -takefocus -xscrollcommand -yscrollcommand
    -closeenough -confine -height (scroll-region -scrollregion) -width 
    -xscrollincrement -yscrollincrement)
  (:default-initargs
      :xscrollcommand (c-in nil)
    :yscrollcommand (c-in nil)
    :id (gentemp "CV")))

(deftk arc (item)
  ()
  (:tk-spec arc
    -dash
    -activedash
    -disableddash
    -dashoffset
    (tk-fill -fill)
    -activefill
    -disabledfill
    -offset
    -outline
    -activeoutline
    -disabledoutline
    -outlinestipple
    -activeoutlinestipple
    -disabledoutlinestipple
    -stipple
    -activestipple
    -disabledstipple
    -state
    -tags
    -width
    -activewidth
    -disabledwidth
    -extent -start -style))

(deftk bitmap (item)
  ()
  (:tk-spec bitmap
    -state -tags
    -anchor
    -background
    -activebackground
    -disabledbackground
    -bitmap
    -activebitmap
    -disabledbitmap
    -foreground
    -activeforeground
    -disabledforeground))


(deftk image (item)
  ()
  (:tk-spec image
    -state
    -tags
    -anchor
    -image
    -activeimage
    -disabledimage))

(deftk line (item)
  ()
  (:tk-spec line
    -dash
    -activedash
    -disableddash
    -dashoffset
    (tk-fill -fill)
    -activefill
    -disabledfill
    -stipple
    -activestipple
    -disabledstipple
    -state
    -tags
    -width
    -activewidth
    -disabledwidth
    -arrow -arrowshape -capstyle -joinstyle -smooth -splinesteps))

(deftk oval (item)
  ()
  (:tk-spec oval
    -dash
   -activedash
   -disableddash
   -dashoffset
   (tk-fill -fill)
   -activefill
   -disabledfill
   -offset
   -outline
   -activeoutline
   -disabledoutline
   -outlinestipple
   -activeoutlinestipple
   -disabledoutlinestipple
   -stipple
   -activestipple
   -disabledstipple
   -state
   -tags
   -width
   -activewidth
   -disabledwidth))

(deftk polygon (item)
  ()
  (:tk-spec polygon
    -dash
    -activedash
    -disableddash
    -dashoffset
    (tk-fill -fill)
    -activefill
    -disabledfill
    -offset
    -outline
    -activeoutline
    -disabledoutline
    -outlinestipple
    -activeoutlinestipple
    -disabledoutlinestipple
    -stipple
    -activestipple
    -disabledstipple
    -state
    -tags
    -width
    -activewidth
    -joinstyle -smooth -splinesteps))

(deftk rectangle (item)
  ()
  (:tk-spec rectangle
    -dash
   -activedash
   -disableddash
   -dashoffset
   (tk-fill -fill)
   -activefill
   -disabledfill
   -offset
   -outline
   -activeoutline
   -disabledoutline
   -outlinestipple
   -activeoutlinestipple
   -disabledoutlinestipple
   -stipple
   -activestipple
   -disabledstipple
   -state
   -tags
   -width
   -activewidth
   -disabledwidth))

(deftk text-item (item)
  ()
  (:tk-spec text
    (tk-fill -fill)
    -activefill
    -disabledfill
    -stipple
    -activestipple
    -disabledstipple
    -state
    -tags
    ;; -- special ---
    -anchor
    -font
    -justify
    -text
    -underline
    -width))

