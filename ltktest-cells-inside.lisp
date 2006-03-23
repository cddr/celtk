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
      :kids (c?
             ; c? has one hell of an expansion. In effect one gets:
             ;   - a first-class anonymous function with the expected body, which will have access to
             ;   - variables self and .cache (symbol macro, last I looked) for the instance and prior
             ;     computed value, if any
             ;   - guaranteed recomputation when the value of any other cell used in the computation changes
             ;
             ; The abbreviation-challenged use c-formula instead of c?, with different syntax I do not recall
             ;
             (the-kids
                 ;
                 ; Cells GUIs get a lot of mileage out of the family class, which is perfect
                 ; for graphical hierarchies. The deets of the-kids are of negligible interest.
                 ;
                 (ltk-test-menus) ;; hiding some code. see defun below for deets
                 (mk-scroller
                  ;
                  ; These "mk-" functions do nothing but expand into (make-instance 'scroller <the initarg list>).
                  ; Where you see, say, mk-button-ex (a) I am poking fun at Microsoft naming of second generation
                  ; library code that did not want to break existing code and (b) adding a little more value (just
                  ; inspect the macro source to see how).
                  ;
                  :packing (c?pack-self "-side top -fill both -expand 1")
                  ;
                  ; Here is an example of how the Family class helps. The above is one of only two packing
                  ; statements need to recreate the ltktest demo. Other packing is handled via two
                  ; slots in an inline-mixin class for various family subclasses, kids-layout and
                  ; kids-packing. The latter pulls any packing parameters and all kids into one
                  ; big pack statement kicked off by an observer on that slot. See the inline-mixin
                  ; class to see how this works.
                  ;
                  ; See the scroller class to see some automation of grids (but this was my first experience
                  ; with grids so look for that to get enhanced over time -- and later automation
                  ; of the use of PLACE.
                  ;
                  :canvas (c? (make-kid 'ltk-test-canvas))) ;; hiding some code. see defmodel thereof below
                  ;
                  ; My bad. Scroller should not assume a canvas is the scrollee. To be refined.
                  ;
                  
                 
                 (mk-row (:packing (c?pack-self "-side bottom"))
                   ;
                   ; Just expand mk-row to see what is going on. It is pretty neat in one respect: if the
                   ; first row parameter is a string, it knows to make a labelframe instead of plain frame)
                   ; The other thing it does, by forcing row parameters into a sub-list as the first argument,
                   ; is let the programmer then just list other widgets (see next) which are understood to
                   ; be subwidgets contained (packed or gridded) within the frame.
                   ;
                   (mk-row (:borderwidth 2 :relief 'sunken)
                     (mk-label :text "Rotation:")
                     (mk-button-ex ("Start" (setf (moire-spin (fm^ :moire-1)) t)))
                     ;
                     ; You were warned about mk-button-ex and its ilk above.
                     ;
                     ; fm^ is a wicked abbreviation for (hey, this is open source, look it up or
                     ; macroexpand it). The long story is that the Family tree becomes effectively
                     ; a namespace, where the ID slot is the name of a widget. I have a suite of
                     ; routines that search the namespace by name so one widget can operate on or,
                     ; more commonly, ask for the value of a slot of some specific widget known to
                     ; be Out There somewhere. (Kids know their parents, so the search can reach
                     ; anywhere in the tree.)
                     ;
                     ; OK, now what is going on here? The above command starts the canvas display
                     ; spinning, by tweaking the "repeat" slot of a "moire" (new ad hoc class) object 
                     ; I created to render the pretty design from
                     ; ltktest. How it accomplishes that will be explained below in the moire class
                     ; definition.
                     ;
                     (mk-button-ex ("Stop" (setf (moire-spin (fm^ :moire-1)) nil))))


                   (mk-button-ex ("Hallo" (format T "~&Hallo")))
                   (mk-button-ex ("Welt!" (format T "~&Welt")))
                   (mk-row (:borderwidth 2
                             :relief 'sunken)
                     (mk-label :text "Test:")
                     (mk-button-ex ("OK:" (setf (moire-spin (fm^ :moire-1)) 20))))
                   (mk-entry :id :entry)
                   (mk-button-ex ("get!" (format t "~&content of entry: ~A" (fm^v :entry))))
                   ;
                   ; fm^v -> (md-value (fm^ ....
                   ;
                   ; The idea being that every Cells model object has an md-value slot bearing the value
                   ; of the thing being modeled. Here, the entry widget is modelling a place for users
                   ; to supply information to an application, and the md-value slot is a good place to
                   ; keep that information.
                   ;
                   ; Thus each class uses md-value to hold something different, but in all cases it is
                   ; the current value of whatever the instance of that class is understood to hold.
                   ;
                   (mk-button-ex ("set!" (setf (fm^v :entry) "test of set"))))))))
   


(defmodel ltk-test-canvas (canvas)
  ()
  (:default-initargs
      :id :test-canvas
    :scroll-region '(0 0 500 400)
    :gridding "-row 0 -column 0 -sticky news"
    ;
    ; As with packing, Celtk tries to simplify life with Tk gridding. But that is achieved partly
    ; by automating things as with the kids-packing and kids-layout slots, and partly by staying
    ; out of the programmer's way and letting them specify actual Tk code to be passed unfiltered
    ; to Tk. The design choice here is to acknowledge that LTk and Celtk users really are still
    ; doing Tk programming; only some automation (and Lispification) is provided.
    ;
    ; This also simplifies Celtk since it just has to pass the Tk code along with "grid <path> "
    ; appended.
    ;
    :xscrollcommand (c-in nil) ;; see canvas class for the Tk limitation behind this nonsense
    :yscrollcommand (c-in nil) ;; in brief, Tk needs the concept of "late binding" on widget names

    :bindings (c? (list (list "<1>" (lambda (event) 
                                      ;
                                      ; Stolen from the original. It means "when the left button is
                                      ; pressed on this widget, popup this menu where the button was pressed"
                                      ;
                                      (pop-up (car (^menus)) ;; (^menus) -> (menus self)
                                        (event-root-x event)
                                        (event-root-y event))))))
    ;
    ; an observer on the bindings slot (a) registers a callback and (b) passes along
    ; to Tk an appropriate BIND command
    ;
    :menus
    ;
    ; here is a limitation with the declarative paradigm. pop-up menus are free to float about
    ; unpacked in any parent. One just needs to remember the name of the menu widget to
    ; pass it to the pop-up function. So imperative code like ltktest original can just make the menus
    ; saving their name in a local variable and then refer to them in a callback to pop them up.
    ;
    ; in the declarative paradigm we need a slot (defined for any widget or item class) in which
    ; to build and store such menus:
    ;
    (c? (the-kids
         (mk-menu 
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
  ((rotx :initarg :rotx :accessor rotx :initform (c-in 0)))
  (:default-initargs
      :timers (c? (list (make-instance 'timer
                            :state (c-in :on)
                            :repeat (c-in nil)
                            :delay 25 ;; milliseconds since this gets passed to TK after
                            :action (lambda (timer)
                                      (when (eq (state timer) :on)
                                        (incf (^rotx)))))))
    :coords (c? (let* ((angle (* 0.1 (^rotx)))
                       (angle2 (* 0.3 angle))
                       (wx (sin (* 0.1 angle))))
                  (loop for i below 100
                      for w = (+ angle (* i 2.8001))
                      for x = (+ (* 50 (sin angle2)) 250 (* 150 (sin w) (1+ wx)))
                      for y = (+ (* 50 (cos angle2)) 200 (* 150 (cos w)))
                      nconcing (list x y))))))

(defun (setf moire-spin) (repeat self)
  (setf (repeat (car (timers self))) repeat))

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

