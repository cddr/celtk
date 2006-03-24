#|

 This software is Copyright (c) 2003, 2004, 2005, 2006  Peter Herth <herth@peter-herth.de>
 Parts Copyright (c) 2005 Thomas F. Burdick
 Parts Copyright (c) Cadence Design Systems, GmbH

 Peter Herth grants you the rights to distribute
 and use this software as governed by the terms
 of the Lisp Lesser GNU Public License
 (http://opensource.franz.com/preamble.html),
 known as the LLGPL.
 
 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!             PROMINENT NOTICE      !!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!                                   !!!!!!!!!!!!!!!
!!!!!!!!!!!! This demo was translated to Cells !!!!!!!!!!!!!!!
!!!!!!!!!!!! by ken Tilton on March 22, 2006.  !!!!!!!!!!!!!!!
!!!!!!!!!!!!                                   !!!!!!!!!!!!!!!
!!!!!!!!!!!! Original (ltktest) can be found   !!!!!!!!!!!!!!!
!!!!!!!!!!!! at the end of ltk.lisp            !!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

|#


(in-package :celtk-user)
#|

The comments throughout this source file cover two broad topics:

    How is programming with Celtk different from LTk?

Contrast the code below with the excellent ltktest "classic" in ltk.lisp to 
see how Celtk programming is different.

Second topic:

    How is programming with Cells different from without Cells?

Those questions are different because not everything different about Celtk
depends on Cells. 

The pattern will be to have explanatory comments appear after the explained code.

n.b. The paint is very fresh on Celtk, so if something like the Timer class looks
dumb, well, it may be. Example: the scroller class only scrolls a canvas (well, I have not tried
supplying a frame for the canvas slot, maybe it would work, but the slot name at least is 
certainly wrong (or the class should be canvas-scroller).

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
  ; Tk does not like Step 3 going before Step 2. That is, .y will not learn about "Hi, Mom.".
  ; Unfortunately, in a declarative paradigm one does not specify in what order different 
  ; things should happen, one just specifies the things we want to have happen. That is 
  ; a big win when it works. But when it did not work for Tk I added to Cells the concept 
  ; of a "client queue" where client-code could store
  ; order-sensitive tasks, also allowing the client to specify the handler for
  ; that queue. This handler gets called at just the right time in the larger scheme of
  ; state propagation one needs for data integrity. What is that?
  ;
  ; Data integrity: when the overall data model gets perturbed by imperative code 
  ; (such as code processing an event loop) executing a SETF of some datapoint X , we want
  ; these requirements satisfied:
  ;
  ;   - all state computed off X (directly or indirectly through some intermediate datapoint) must be recomputed;
  ;   - recomputations must see only datapoint values current with the new value of X. This must
  ;     work transparently, ie, datapoint accessors are responsible for returning only current values;
  ;   - similarly, client observers ("on change" callbacks) must see only values current with the new value of X
  ;   - a corrollary: should a client observer SETF a datapoint Y, all the above must
  ;     happen with values current not just with X, but also with the value of Y /prior/
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
  ((entry-warning :reader entry-warning
     :initform (c? (bwhen (bad-chars (loop for c across (fm!v :entry)
                                           when (digit-char-p c)
                                           collect c))
                     (format nil "Please! No digits! I see ~a!!" bad-chars)))
     ;
     ; By the time I decided to add this demo I already had a long discussion under the get! and set! buttons, so
     ; check those out for details.
     ;
     :documentation "Demonstrate live tracking of entry edit"))

  (:default-initargs
      :id :ltk-test
      :kids (c?
             ; c? has one hell of an expansion. In effect one gets:
             ;   - a first-class anonymous function with the expected body, which will have access to
             ;   - variables self and .cache (symbol macro, last I looked) for the instance and prior
             ;     computed value, if any
             ;   - guaranteed recomputation when the value of any other cell used in the computation changes
             ;
             ; If the abbreviation bothers you, look up c-formula.
             ;
             (the-kids
                 ;
                 ; Cells GUIs get a lot of mileage out of the family class, which is perfect
                 ; for graphical hierarchies. "the-kids" does not do much, btw.
                 ;
                 (ltk-test-menus) ;; hiding some code. see defun below for deets
                 (mk-scroller
                  ;
                  ; These "mk-" functions do nothing but expand into (make-instance 'scroller <the initarg list>).
                  ; Where you see, say, mk-button-ex I am (a) poking fun at Microsoft naming of second generation
                  ; library code that did not want to break existing code and (b) adding a little more value (just
                  ; inspect the macro source to see how).
                  ;
                  :packing (c?pack-self "-side top -fill both -expand 1")
                  ;
                  ; Here is an example of how the Family class helps. The above is one of only two packing
                  ; statements needed to recreate the ltktest demo. Other packing is handled via two
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
                   ; be kids/subwidgets contained (packed or gridded) within the frame.
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
                     ; spinning, by tweaking (via the (setf moire-spin) defun below) the "repeat" slot of 
                     ; an ad hoc "moire" class object created to render the pretty design from
                     ; ltktest. How it accomplishes that will be explained below in the moire class
                     ; definition.
                     ;
                     (mk-button-ex ("Stop" (setf (moire-spin (fm^ :moire-1)) nil))))
                     ;
                     ; ditto
                     ;


                   (mk-button-ex ("Hallo" (format T "~&Hallo")))
                   (mk-button-ex ("Welt!" (format T "~&Welt")))
                   (mk-row (:borderwidth 2
                             :relief 'sunken)
                     (mk-label :text "Test:")
                     (mk-button-ex ("OK:" (setf (moire-spin (fm^ :moire-1)) 20))))
                   (mk-entry :id :entry
                     :background (c? (if (entry-warning .tkw)
                                         ;
                                         ; ok, this is silly, the validation is entry-specific
                                         ; and should be a rule applied to this entry widget, but I 
                                         ; will leave it silly to make clear that cells of an instance
                                         ; can depend on cells of other instances
                                         ;
                                         ; so what is .tkw? A symbol macro for (nearest self window).
                                         ; what is nearest? It searches up the Family tree from
                                         ; self inclusive searching for something (typep 'window)
                                         ;
                                       "red"
                                       'SystemButtonFace)))
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
                   ; The interesting question is, how does the md-value slot of the Lisp instance stay
                   ; current with the text being edited in the Tk entry widget? Here we have a fundamental
                   ; difference between Ltk and Celtk. Ltk lets Tk take care of everything, including
                   ; storing the data. eg, (text my-entry) is an accessor call that asks Tk the value of
                   ; the -text configuration for the Tk instance mirrored by my-entry. There is no text
                   ; slot in the Lisp entry instance. But Cells works
                   ; by having datapoints watching other datapoints, so we want data in the Lisp domain
                   ; changing automatically as it changes on the TK side (such as when the user is actually
                   ; typing in the entry widget). See the entry class to see how it uses the TCL "trace write"
                   ; mechanism to keep the Lisp md-value slot abreast of the Tk entry text configuration
                   ; keystroke by keystroke. 
                   ; 
                   ; I just added the entry-value slot above to demonstrate the mechanism in action. Click
                   ; on the entry widget and type "abc123", then delete the 3, 2, and 1, keeping an eye
                   ; on standard output.
                   ;
                   (mk-button-ex ("set!" (setf (fm^v :entry) "test of set")))
                   ;
                   ; In Ltk one would SETF (text my-entry) and the
                   ; SETF method would communicate with Tk to make the change to the Tk widget -text
                   ; configuration. In Celtk, the md-value slot of the entry gets changed (possibly
                   ; triggering other slots to update, which is why we do not just talk to Tk) and
                   ; then that value gets propagated to Tk via "set <widget path> <value>". Because
                   ; the textVariable for every entry is the entry itself, the text of the entry
                   ; then changes. If that sounds weird, what we are actually doing is tapping into
                   ; Tk to a large degree taking the same approach as Cells does with the md-value
                   ; slot: in Cells, we think of model instances as wrapping some model-specific 
                   ; value, which is held in the md-value slot of the model instance. Tk simply
                   ; allows a widget path to be a global variable. Furthermore, as the company name
                   ; ActiveState suggests, Tk also provides automatic propagation: change the
                   ; variable, and anyone with that as its textVariable also changes.
                   )))))
   
(defobserver entry-warning ()
    ;
    ; This demonstrates ones ability to track the text in a Tk entry while it is being
    ; edited. As you type you should see the changing values in standard output
    ;
    (if new-value
        (format t "~&User, we have a problem: ~a" new-value)
      (when old-value
        (format t "~&That looks better: ~a" (fm!v :entry)))))

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
    ; pass it to the pop-up function. So imperative code like ltktest "classic" can just make the menus
    ; saving their name in a closed-over local variable and then refer to them in a callback to pop them up.
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
               ;
               ; we give /this/ widget a specific ID so other rules can look it up, as
               ; discussed above when explaining fm^.
  
(defmodel moire (line)
  ((angle-1 :initarg :angle-1 :accessor angle-1 :initform (c-in 0)))
  (:default-initargs
      :timers (c? (list (make-instance 'timer
                          ;
                          ; it occurred to me that it might be useful to build a timer utility
                          ; around the TCL after command. See the class definition of timer
                          ; for the fireworks (in terms of Cells) that resulted
                          ;
                          :repeat (c-in nil)
                          :delay 25 ;; milliseconds since this gets passed unvarnished to TK after
                          :action (lambda (timer)
                                    (when (eq (state timer) :on)
                                      (incf (^angle-1) 0.1))))))
    :coords (c? (let* ((angle-2 (* 0.3 (^angle-1)))
                       (wx (sin (* 0.1 (^angle-1)))))
                  (loop for i below 100
                      for w = (+ (^angle-1) (* i 2.8001))
                      for x = (+ (* 50 (sin angle-2)) 250 (* 150 (sin w) (1+ wx)))
                      for y = (+ (* 50 (cos angle-2)) 200 (* 150 (cos w)))
                      nconcing (list x y))))))

(defun (setf moire-spin) (repeat self)
  (setf (repeat (car (timers self))) repeat)) ;; just hiding the implementation

(defun ltk-test-menus ()
  ;
  ; The only difference is that the menu structure as seen by the user
  ; is apparent here, which might help some when reorganizing menus.
  ;
  ; Well, another thing which happens not to be visible here... hang on.
  ; OK, I just made the Save menu item contingent upon there being no 
  ; entry-warning. As you add/remove all digits (considered invalid for
  ; demonstration purposes) the menu item becomes available/unavailable
  ; appropriately.
  ;
  ; This is the kind of thing that Cells is good for.
  ;
  (mk-menubar
   :kids (c? (the-kids
              (mk-menu-entry-cascade-ex (:label "File")
                (mk-menu-entry-command :label "Load"
                  :command (c? (tk-callback .tkw 'load
                                 (lambda () (format t "~&Load pressed")))))
                                      
                (mk-menu-entry-command :label "Save"
                  :state (c? (if (entry-warning (fm^ :ltk-test))
                                 :disabled :normal))
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
                  :accelerator "Alt-q"
                  ;
                  ; check out the observer on the accelerator slot of the class menu-entry-usable
                  ; to see how Celtk fills in a gap in Tk: accelerators should work just by
                  ; declaring them to the menu widget, it seems to me. In Celtk, they do.
                  ;
                  :underline 1
                  :command "exit"))))))


