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


(in-package :celtk-user)

(defun ctk::tk-test () ;; ACL project manager needs a zero-argument function, in project package
  (test-window 
   ;; true tester: 'one-button-window
   ;; Not so good: 'ltktest-cells-inside
   ;; 'menu-button-test
   ;; 'spinbox-test
    'lotsa-widgets
   ;; Now in Gears project 'gears-demo
  ))

(defmodel one-button-window (window)
  ()
  (:default-initargs
    :kids (c? (the-kids                
               (mk-frame-stack
                :packing (c?pack-self)
                :kids (c? (the-kids
                           (make-instance 'entry
                             :id :entree
                             :fm-parent *parent*
                             :md-value (c-in "Boots"))
                           (make-instance 'button
                             :fm-parent *parent*
                             :text "read"
                             :on-command (lambda (self)
                                           (trc "entry reads" (ctk::tk-eval-var (path (fm^ :entree)))))))))))))

#+save
(defmodel one-button-window (window)
  ()
  (:default-initargs
      :on-event (lambda (self &rest event-args)
                  (trc "we got events" self event-args))
    :kids (c? (the-kids                
               (mk-menubar
                :kids (c? (the-kids
                           (mk-menu-entry-cascade-ex (:label "File")
                             (mk-menu-entry-command-ex () "Load" (format t "~&Load pressed"))
                             (mk-menu-entry-command-ex () "Save" (format t "~&Save pressed"))))))
               (mk-frame-stack
                :packing (c?pack-self)
                :kids (c? (the-kids
                           
                           ;;;                           (mk-scrolled-list
                           ;;;                            :id :spinpkg-sym-list
                           ;;;                            :list-height 6
                           ;;;                            :list-item-keys (c? (loop for sym being the symbols in (find-package "CELTK")
                           ;;;                                                    for n below 5
                           ;;;                                                    counting sym into symct
                           ;;;                                                    collecting sym into syms
                           ;;;                                                    finally (trc "syms found !!!" symct)
                           ;;;                                                      (return syms)))
                           ;;;                            :list-item-factory (lambda (sym)
                           ;;;                                                 (trc "make list item" sym *parent*)
                           ;;;                                                 (make-instance 'listbox-item
                           ;;;                                                   :fm-parent *parent*
                           ;;;                                                   :md-value sym
                           ;;;                                                   :item-text (down$ (symbol-name sym)))))
                           (mk-text-widget
                            :id :my-text
                            :md-value (c?n "hello, world")
                            :height 3
                            :width 25)
                           (make-instance 'button
                             :fm-parent *parent*
                             :text "<<kenny>>"
                             :on-command (lambda (self)
                                           (trc "button pushed!!!" self)))
                           ;;;                           (make-instance 'button
                           ;;;                             :fm-parent *parent*
                           ;;;                             :text "time now?"
                           ;;;                             :on-command (c? (lambda (self)
                           ;;;                                               (trc "we got callbacks" self))))
                           (make-instance 'scale
                             :fm-parent *parent*
                             :tk-label "Boots"
                             :on-command (c? (lambda (self value)
                                               (trc "we got scale callbacks" self value))))
                           (mk-spinbox
                            :id :spin-pkg
                            :md-value (c-in "cells") ;;(cells::c?n "cells")
                            :tk-values (mapcar 'down$
                                         (sort (mapcar 'package-name
                                                 (list-all-packages))
                                           'string>)))
                           (make-instance 'entry
                             :fm-parent *parent*
                             :md-value (c-in "Boots"))
                           )))))))

(defmodel spinbox-test (window)
  ()
  (:default-initargs
      :kids (c? (the-kids
                 (mk-stack (:packing (c?pack-self))
                   (mk-spinbox
                    :id :spin-pkg
                    :md-value (c-in "cells") ;;(cells::c?n "cells")
                    :tk-values (mapcar 'down$
                                 (sort (mapcar 'package-name
                                         (list-all-packages))
                                   'string>)))
                   (mk-scrolled-list
                    :id :spinpkg-sym-list
                    :list-height 6
                    :list-item-keys (c? (trc "enter item keys" self (fm^ :spin-pkg))
                                      (let* ((spinner (fm^ :spin-pkg))
                                               (item (when spinner (md-value spinner)))
                                               (pkg (find-package (string-upcase item))))
                                          (when pkg
                                            (loop for sym being the symbols in pkg
                                                for n below 5
                                                counting sym into symct
                                                collecting sym into syms
                                                finally (return syms)))))
                    :list-item-factory (lambda (sym)
                                         (make-instance 'listbox-item
                                           :fm-parent *parent*
                                           :md-value sym
                                           :item-text (down$ (symbol-name sym))))))))))


(defmodel menu-button-test (window)
  ()
  (:default-initargs
      :kids (c? (the-kids
                 (mk-stack ("Style by Widgets" :id :widstyle :packing (c?pack-self))
                   (mk-popup-menubutton
                    :id :font-face
                    :initial-value (c? (second (^entry-values)))
                    :entry-values (c? (subseq (tk-eval-list "font families") 4 10)))
                   (mk-label :text "Four score and seven years ago today, our fathers broguht forth on this continent a new nation..."
                     :wraplength 200
                     :justify 'left
                     :tkfont (c? (list
                                  (selection (fm^ :font-face))
                                  14))))))))
 
(defmodel font-view-2 (window)
  ()
  (:default-initargs
      :kids (c? (the-kids
                 (mk-panedwindow
                  :packing (c?pack-self)
                  :orient 'vertical
                  :kids  (c? (the-kids
                              (loop repeat 2
                                  collecting (make-instance 'font-view :fm-parent *parent*)))))))))

(defun mk-font-view ()
  (make-instance 'font-view))

(defmodel font-view (frame-stack)
  ()
  (:default-initargs
      :md-value (c? (tk-eval-list "font families"))
    :pady 2 :padx 4
    :packing-side 'left
    :layout-anchor 'nw
    :kids (c? (the-kids
               (mk-spinbox :id :font-face
                 :md-value (c-in (car (^md-value)))
                 :tk-values (c? (md-value .parent)))
               (mk-scale :id :font-size
                 :md-value (c-in 14)
                 :tk-label "Font Size"
                 :from 7 :to 24 
                 :orient 'horizontal)
               (mk-label :id :txt
                 :text "Four score seven years ago today"
                 :wraplength 600
                 :tkfont (c? (list ;; format nil "{{~{~a~^ ~}} ~a}" ;; eg, {{wp greek century} 24}
                            (md-value (fm^ :font-face))
                            (md-value (fm^ :font-size)))))))))

#| 06-02-14 following stuff not resurrected after latest revisions to Celtk

;;; ---- toplevel --------------------------------




(defmodel file-open (toplevel)
  ()
  (:default-initargs
    :md-value (c? (directory "\\windows\\fonts\\*.ttf"))
    :pady 2 :padx 4
    :kids  (c? (the-kids
                (mk-spinbox :id :font-face
                  :md-value (c-in (car (^md-value)))
                 :tk-values (c? (mapcar 'pathname-name (md-value .parent))))
                (mk-button-ex ("Open" (progn
                                     (tk-format `(:destroy ,self) "destroy ~a" (path (upper self toplevel)))
                                     (not-to-be (upper self toplevel))))
                 :underline 0)))))

|#