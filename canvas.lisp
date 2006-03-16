#|

 Celtic / frame.lisp

 Copyright (c) 2004 by Kenneth William Tilton <ktilton@nyc.rr.com>

 A work derived from Peter Herth's LTk. As a derived work,
 usage is governed by LTk's "Lisp LGPL" licensing:

 You have the right to distribute and use this software as governed by 
 the terms of the Lisp Lesser GNU Public License (LLGPL):

    (http://opensource.franz.com/preamble.html)
 
 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 Lisp Lesser GNU Public License for more details.
 
|#

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

