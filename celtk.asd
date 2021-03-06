;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;; adjust the following form and uncomment to adjust 
;; optimization parameters

;; #+(or allegro lispworks cmu mcl clisp cormanlisp sbcl scl)
;; (progn
;;   (declaim (optimize (debug 3) (speed 0) (safety 1) (compilation-speed 0))))

(asdf:defsystem :celtk
    :name "celtk"
  :author "Kenny Tilton <kentilton@gmail.com>"
  :version "2.0"
  :maintainer "Kenny Tilton <kentilton@gmail.com>"
  :licence "Lisp LGPL"
  :description "Tcl/Tk with Cells Inside(tm)"
  :long-description "A Cells-driven portable GUI, ultimately implmented by Tcl/Tk"
  :depends-on (:cells :cffi :gui-geometry)
  :serial t
  :components ((:file "packages")
	       (:file "libraries")
	       (:file "celtk")
               (:file "tk-structs")
               (:file "tk-interp")
               (:file "tk-events")
               (:file "tk-object")
               (:file "widget")
               (:file "layout")
               (:file "font")
               (:file "timer")
               (:file "menu")
               (:file "label")
               (:file "entry")
               (:file "button")
               (:file "multichoice")
               (:file "scroll")
               (:file "canvas")
               (:file "text-item")
               (:file "item-pictorial")
               (:file "item-shaped")
               (:file "composites")
               (:file "frame")
               (:file "fileevent")
               (:file "run")
	       (:file "run-celtk-window")
	       (:file "ltktest-ci")
               (:file "lotsa-widgets")
               (:file "demos")))
