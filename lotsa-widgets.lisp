(in-package :celtk-user)

(defmodel lotsa-widgets (window)
  ()
  (:default-initargs
      :kids (c? (the-kids
                 (demo-all-menubar)
                 
                 (mk-row (:packing (c?pack-self))
                   (mk-label :text "aaa"
                     :image-files (list (list 'kt (make-pathname #+lispworks :host #-lispworks :device "c"
                                                    :directory '(:absolute "0dev" "Celtk")
                                                    :name "kt69" :type "gif")))
                     :height 200
                     :width 300
                     :image (c? (format nil "~(~a.~a~)" (ctk::^path) 'kt)))
                   
                   (assorted-canvas-items)
                   
                   (mk-stack ()
                     (mk-text-widget
                      :id :my-text
                      :md-value (c?n "hello, world")
                      :height 8
                      :width 25)
                     
                     (spin-package-with-symbols))
                   
                   (mk-stack ()
                     (mk-row (:id :radio-ny :selection (c-in 'yes))
                       (mk-radiobutton-ex ("yes" 'yes))
                       (mk-radiobutton-ex ("no" 'no))
                       (mk-label :text (c? (string (selection (upper self selector))))))
                     (mk-row ()
                       (mk-checkbutton :id :check-me
                         :text "Check Me"
                         :md-value (c-in t))
                       (mk-label :text (c? (if (fm^v :check-me) "checked" "unchecked"))))
                     (mk-row ()
                       (mk-button-ex ("Time now?" (setf (fm^v :push-time)
                                                    (get-universal-time))))
                       (mk-label :text (c? (time-of-day (^md-value)))
                         :id :push-time
                         :md-value (c-in (get-universal-time))))
                     
                     (style-by-edit-menu)
                     
                     (style-by-widgets)
                     
                     (mk-row (:layout-anchor 'sw)
                       (mk-entry
                        :id :enter-me)
                       (mk-label :text (c? (conc$ "echo " (fm^v :enter-me))))))
                   
                   (duelling-scrolled-lists)
                   )))))

 
(defun style-by-edit-menu ()
    (mk-row ("Style by Edit Menu")
      (mk-label :text "Four score and seven years ago today"
        :wraplength 600
        :tkfont (c? (list
                   (selection (fm^ :app-font-face))
                   (selection (fm^ :app-font-size))
                   (if (fm^v :app-font-italic)
                       'italic 'roman)
                   (if (fm^v :app-font-bold)
                       'bold 'normal))))))

(defun spin-package-with-symbols ()
  (mk-stack ()
    (mk-spinbox
     :id :spin-pkg
     :md-value (cells::c?n "cells")
     :tk-values (mapcar 'down$
                  (sort (mapcar 'package-name
                          (list-all-packages))
                    'string>)))
    (mk-scrolled-list
     :id :spinpkg-sym-list
     :list-height 6
     :list-item-keys (c? (let* ((spinner (fm^ :spin-pkg))
                                (item (when spinner (md-value spinner)))
                                (pkg (find-package (string-upcase item))))
                           (when pkg
                             (loop for sym being the symbols in pkg
                                   for n below 25
                                   counting sym into symct
                                   collecting sym into syms
                                   finally (trc "syms found !!!" symct)
                                   (return syms)))))
     :list-item-factory (lambda (sym)
                          (make-instance 'listbox-item
                            :fm-parent *parent*
                            :md-value sym
                            :item-text (down$ (symbol-name sym)))))))

(defun duelling-scrolled-lists ()
  (mk-row ()
    (mk-scrolled-list
     :id :pkg-list
     :selection (c-in (find-package "ASDF"))
     :list-height 6
     :list-item-keys (list-all-packages)
     :list-item-factory (lambda (pkg)
                          (make-instance 'listbox-item
                            :fm-parent *parent*
                            :md-value pkg
                            :item-text (down$ (package-name pkg)))))
    (mk-scrolled-list
     :id :pkg-sym-list
     :list-height 6
     :list-item-keys (c? (bwhen (pkg (selection (fm^ :pkg-list)))
                           (loop  for sym being the present-symbols in pkg
                                 for n below 25
                               collecting sym)))
     :list-item-factory (lambda (sym)
                          (make-instance 'listbox-item
                            :md-value sym
                            :fm-parent *parent*
                            :item-text (down$ (symbol-name sym)))))))

(defun assorted-canvas-items ()
  (mk-canvas
   :height 350
   :kids (c? (the-kids
              (mk-bitmap :coords (list 140 140)
                :bitmap "@\\0dev\\Celtk\\x1.xbm" #+not "@\\temp\\gsl.xbm")
              (mk-rectangle :coords (list 10 10 100 60)
                :tk-fill "red")
              (mk-text-item :coords (list 100 80)
                :text "i am an item"
                :tk-fill 'blue)
              (mk-arc :coords (list 10 100 100 160)
                :start 45
                :tk-fill "orange")
              (mk-line :coords (list 250 10 300 40 250 70 400 100)
                :width 8
                :smooth 'bezier
                :joinstyle 'miter
                :arrow 'both
                :tk-fill 'purple)
              (mk-oval :coords (list 10 200 100 260)
                :tk-fill "yellow")
              (mk-polygon :coords (list 250 210 300 220 340 200 260 180)
                :width 4
                :tk-fill 'green
                :smooth 'bezier
                :joinstyle 'miter)
              (mk-arc :coords (list 10 300 100 360)
                :start 45
                :tk-fill "white")
              ))))

(defun style-by-widgets ()
  (mk-stack ("Style by Widgets" :id :widstyle)
    (mk-row (:id :stywid
              :packing-side 'left
              :layout-anchor 'sw)
      (mk-popup-menubutton
       :id :font-face
       :initial-value (c? (second (^entry-values)))
       :entry-values (c? (eko ("popup ff") (subseq (tk-eval-list "font families") 4 10))))
                          
      (mk-scale :id :font-size
        :md-value (c-in 14)
        :tk-label "Font Size"
        :from 7 :to 24 
        :orient 'horizontal))
              
              
    (mk-label :text "Four score and seven years ago today, our fathers broguht forth on this continent a new nation..."
      :wraplength 200
      :justify 'left
      :tkfont (c? (list
                 (selection (fm^ :font-face))
                 (md-value (fm^ :font-size)))))))

(defun demo-all-menubar ()
  (mk-menubar
   :id 'mbar
   :kids (c? (the-kids
              (mk-menu-entry-cascade
               :id 'file
               :label "File"
               :kids (c? (the-kids
                          (mk-menu
                           :id 'filemenu
                           :kids (c? (the-kids
                                      (mk-menu-entry-command :label "New" :command "exit")
                                      (mk-menu-entry-command :label "Open" :command "tk_getOpenFile")
                                      (mk-menu-entry-command :label "Close" :command "exit")
                                      (mk-menu-entry-separator)
                                      (mk-menu-entry-command :label "Quit"
                                        :state (c? (if t ;; (md-value (fm^ :check-me))
                                                       'normal 'disabled))
                                        :command "exit")))))))
              (mk-menu-entry-cascade
               :id 'editcascade
               :label "Edit"
               :kids (c? (the-kids
                          (mk-menu
                           :id 'editmenu
                           :kids (c? (the-kids
                                      (mk-menu-entry-command :label "Undo"
                                        :on-command  (lambda (self) 
                                                         (trc "edit menu undo" self)))
                                      (mk-menu-entry-separator)
                                      (mk-menu-entry-command :label "Cut" :command "exit")
                                      (mk-menu-entry-command :label "Copy" :command "exit")
                                      (mk-menu-entry-command :label "Paste" :command "exit")
                                      (mk-menu-entry-command :label "Clear" :command "exit")
                                      (mk-menu-entry-separator)
                                      (mk-menu-radio-group :id :app-font-face
                                        :selection (c-in "courier")
                                        :kids (c? (the-kids
                                                   (mk-menu-entry-radiobutton :label "Times" :value "times")
                                                   (mk-menu-entry-radiobutton :label "Courier" :value "courier")
                                                   (mk-menu-entry-radiobutton :label "Helvetica" :value "helvetica"))))
                                      (mk-menu-entry-separator)
                                      (mk-menu-entry-cascade
                                       :id :app-font-size
                                       :label "Font Size"
                                       :menu (c? (path (kid1 self)))
                                       :selection (c-in 12)
                                       :kids (c? (the-kids
                                                  (mk-menu
                                                   :id :fsztoff
                                                   :tearoff 1
                                                   :kids (c? (the-kids
                                                              (loop for (label value) in '(("9" 9)("12" 12)("14"  14))
                                                                  collecting (mk-menu-entry-radiobutton :label label :value value))))))))
                                      (mk-menu-entry-separator)
                                      (mk-menu-entry-checkbutton :id :app-font-italic :label "Italic")
                                      (mk-menu-entry-checkbutton :id :app-font-bold :label "Bold" :md-value (c-in t))))))))))))

