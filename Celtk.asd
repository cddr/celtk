;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

#+(or allegro lispworks cmu mcl clisp cormanlisp sbcl scl)
(progn
  (declaim (optimize (debug 3) (speed 3) (safety 1) (compilation-speed 0))))

(asdf:defsystem :celtk
  :name "celtk"
  :author "Kenny Tilton <ktilton@nyc.rr.com>"
  :version "2.0"
  :maintainer "Kenny Tilton <ktilton@nyc.rr.com>"
  :licence "MIT Style"
  :description "Tk via LTk with Cells Inside(tm)"
  :long-description "A Cells-driven portable GUI built atop the LTk core, ultimately implmented by Tk"
  :depends-on (:ltk :cells)
  :serial t
  :components ((:file "Celtk")
               (:file "tk-format")
               (:file "menu")
               (:file "textual")
               (:file "widgets")
               (:file "canvas")
               (:file "composites")
               (:file "demos")
               (:file "ltktest-cells-inside")))

