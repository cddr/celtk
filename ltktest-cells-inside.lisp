(in-package :celtk-user)

#+test-ltktest
(progn
  (cells-reset 'tk-user-queue-handler)
  (tk-test-class 'ltktest-cells-inside))

(defmodel ltktest-cells-inside (window)
  ((elapsed :initarg :elapsed :accessor elapsed :initform (c-in 0)))
  (:default-initargs
      :kids (c? (the-kids
                 (ltk-test-menus)
                 (mk-scroller
                    :packing (c?pack-self "-side top -fill both -expand 1")
                    :canvas (c? (make-kid 'ltk-test-canvas)))
                 (mk-row (:packing (c?pack-self "-side bottom"))
                   (mk-row (:borderwidth 2 :relief 'sunken)
                     (mk-label :text "Rotation:")
                     (mk-button-ex ("Start" (setf (repeat (fm^ :moire-1)) t)))
                     (mk-button-ex ("Stop" (setf (repeat (fm^ :moire-1)) nil))))
                   (mk-button-ex ("Hallo" (format T "~&Hallo")))
                   (mk-button-ex ("Welt!" (format T "~&Welt")))
                   (mk-row (:borderwidth 2
                             :relief 'sunken)
                     (mk-label :text "Test:")
                     (mk-button-ex ("OK:" (setf (repeat (fm^ :moire-1)) (make-timer-steps :count 20)))))
                   (mk-entry :id :entry)
                   (mk-button-ex ("get!" (format t "~&content of entry: ~A" (fm^v :entry))))
                   (mk-button-ex ("set!" (setf (fm^v :entry) "test of set"))))))))
   
(defmodel ltk-test-canvas (canvas)
  ()
  (:default-initargs
      :id :test-canvas
    :scroll-region '(0 0 500 400)
    :gridding "-row 0 -column 0 -sticky news"
    :xscrollcommand (c-in nil) ;; see initialize-instance of canvas for gory details
    :yscrollcommand (c-in nil)
    :bindings (c? (list (list "<1>" (lambda (event)
                                      (pop-up (car (^menus))
                                        (event-root-x event)
                                        (event-root-y event))))))
    :menus (c? (the-kids (mk-menu
                          :kids (c? (the-kids
                                     (mapcar (lambda (spec)
                                               (destructuring-bind (lbl . out$) spec
                                                 (mk-menu-entry-command
                                                  :label lbl
                                                  :command (c? (tk-callback .tkw (gentemp "MNU")
                                                                 (lambda ()
                                                                   (format t "~&~a" out$)))))))
                                       (list (cons "Option 1" "Popup 1")
                                         (cons "Option 2" "Popup 2")
                                         (cons "Option 3" "Popup 3"))))))))
    
    :kids (c? (the-kids
               (mk-text-item
                :coords (list 10 10)
                :anchor "nw"
                :text "Ltk Demonstration")
               (make-kid 'moire :id :moire-1)))))
  
(defmodel moire (line)
  ((rotx :initarg :rotx :accessor rotx :initform (c-in 0))
   (repeat :initarg :repeat :accessor repeat :initform (c-in nil)))
  (:default-initargs
      :timers (c? (when (^repeat)
                      (list (make-instance 'timer
                              :tag :moire
                              :delay 1
                              :repeat (let ((m self))
                                        (c? (repeat m)))
                              :action (lambda (timer)
                                        (declare (ignore timer))
                                        (incf (^rotx)))))))
    :coords (c? (let* ((angle (* 0.1 (^rotx)))
                       (angle2 (* 0.3 angle))
                       (wx (sin (* 0.1 angle))))
                  (loop for i below 100
                      for w = (+ angle (* i 2.8001))
                      for x = (+ (* 50 (sin angle2)) 250 (* 150 (sin w) (1+ wx)))
                      for y = (+ (* 50 (cos angle2)) 200 (* 150 (cos w)))
                      nconcing (list x y))))))


(defun ltk-test-menus ()
  (mk-menubar
   :kids (c? (the-kids
              (mk-menu-entry-cascade-ex (:label "File")
                (mk-menu-entry-command :label "Load"
                  :command (c? (tk-callback .tkw 'load
                                 (lambda () (format t "~&Load pressed")))))
                                      
                (mk-menu-entry-command :label "Save"
                  :command (c? (tk-callback .tkw 'save
                                 (lambda () (format t "~&Save pressed")))))
                (mk-menu-entry-separator)
                (mk-menu-entry-cascade-ex (:id :export :label "Export...")
                  (mk-menu-entry-command 
                   :label "jpeg"
                   :command (c? (tk-callback .tkw 'jpeg
                                  (lambda () (format t "~&Jpeg pressed")))))
                  (mk-menu-entry-command
                   :label "png"
                   :command (c? (tk-callback .tkw 'png
                                  (lambda () (format t "~&Png pressed"))))))
                (mk-menu-entry-separator)
                (mk-menu-entry-command :label "Quit"
                  :accelerator "<Alt-q>"
                  :underline 1
                  :command "exit"))))))

