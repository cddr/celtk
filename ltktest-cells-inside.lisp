(in-package :celtk-user)
#|

The comments throughout this source file cover two broad topics:

    How is programming with Celtk different from LTk?
    How is programming with Cells different from without Cells?

Those questions are different because not everything different about Celtk
depends on Cells. 

The pattern will be to have explanatory comments appear after the explained code.

|#
#+test-ltktest
(progn
  (cells-reset 'tk-user-queue-handler)
  ;
  ; Tk is fussy about the order in which things happen. It likes:
  ;    - create widgets .x and .y
  ;    - make .x the -textvariable of .y
  ;    - set .x to "Hi, Mom"
  ;
  ; Tk does not like Step 3 going before Step 2. Unfortunately, in a declarative paradigm
  ; one does not specify in what order different things should happen, one just specifies
  ; the things we want to have happen. That is a big win when it works. But when it did not
  ; I created the concept of a so-called "client queue" where client-code could store
  ; order-sensitive tasks, and then allowed the client also to specify the handler for
  ; that queue. This handler gets called at just the right time in the larger scheme of
  ; state propagation one needs for data integrity. Whassat?
  ;
  ; Data integrity: when the overall data model gets perturbed by a SETF by imperative code 
  ; (usually processing an event loop) of some datapoint X , we need:
  ;
  ;   - all state computed off X (directly or indirectly through some intermediate state) must be recomputed;
  ;   - no recomputation can use datapoints not current with the new value of X;
  ;   - when invoking client observers to process a change in a datapoint, no observer can use
  ;     any datapoint not current with X; and a corrollary:
  ;   - should a client observer itself want to SETF a datapoint Y, all the above must
  ;     happen not just with values current with X, but also current with the value of Y /prior/
  ;     to the intended change to Y.
  ;
  ; To achieve the above, Cells2 and now Cells3 have taken to using FIFO "unfinished business" queues 
  ; to defer things until The Right Time. Which brings us back to Tk. Inspect the source of
  ; tk-user-queue-handler and search the Celtk source for "with-integrity (:client" to see how Celtk
  ; manages to talk to Tk in the order Tk likes. But in short, we just add this requirement:
  ;  
  ;   - Client code must see only values current with X and not any values current with some
  ;     subsequent change to Y queued by an observer
  ; 
  (tk-test-class 'ltktest-cells-inside))

; That is all the imperative code there is to Celtk application development, aside from widget commands. Tk handles some
; of the driving imperative logic, and Celtk internals handle the rest. The application works via rules reacting to change,
; computing new state for the application model, which operates on the outside world via observers (on-change callbacks) triggered
; automatically by the Cells engine. See DEFOBSERVER.

(defmodel ltktest-cells-inside (window)
  ()
  (:default-initargs
      :kids (c? (the-kids
                 ;
                 ; Cells GUIs get a lot of mileage out of the family class, which is perfect
                 ; for graphical hierarchies.
                 ;
                 (ltk-test-menus) ;; hiding some code. see below for deets
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
                              :delay 25
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

