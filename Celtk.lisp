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

(defpackage :celtk
  (:nicknames "CTK")
  (:use :common-lisp :utils-kt :cells)

  (:import-from #:ltk
    #:wish-stream #:*wish* #:widget-path
    #:read-data #:event-root-x #:event-root-y
    #:send-wish #:tkescape #:after #:after-cancel #:bind
    #:with-ltk #:do-execute #:add-callback)

  (:export
    #:pop-up #:event-root-x #:event-root-y
   #:window #:panedwindow #:mk-row #:c?pack-self #:mk-stack #:mk-text-widget
    #:mk-panedwindow
   #:mk-stack #:mk-radiobutton #:mk-radiobutton-ex #:mk-radiobutton #:mk-label #:selection #:selector
    #:mk-checkbutton #:mk-button #:mk-button-ex  #:entry #:mk-entry #:text
    #:frame-stack #:mk-frame-stack #:path #:^path
    #:mk-menu-entry-radiobutton #:mk-menu-entry-checkbutton
    #:mk-menu-radio-group #:mk-menu-entry-separator
    #:mk-menu-entry-command #:tk-callback #:menu #:mk-menu #:^menus #:mk-menu-entry-cascade #:mk-menubar
    #:^entry-values #:tk-eval-list #:mk-scale #:mk-popup-menubutton
    #:polygon #:mk-polygon #:oval #:mk-oval #:line #:mk-line #:arc #:mk-arc #:text-item #:mk-text-item
    #:rectangle #:mk-rectangle #:bitmap #:mk-bitmap #:canvas #:mk-canvas #:mk-frame-row
    #:mk-scrolled-list #:listbox-item #:mk-spinbox
    #:mk-scroller #:mk-menu-entry-cascade-ex
    #:with-ltk #:tk-format #:send-wish #:value #:.tkw
    #:tk-user-queue-handler #:user-errors #:^user-errors
   #:timer #:timers #:repeat #:executions #:state #:timer-reset #:make-timer-steps
   #:^widget-menu #:widget-menu))

(defpackage :celtk-user
  (:use :common-lisp :utils-kt :cells :celtk))

(in-package :Celtk)


(defmodel tk-object (model)
  ((.md-name :cell nil :initform (gentemp "TK") :initarg :id)
   (tk-class :cell nil :initform nil :initarg :tk-class :reader tk-class)
   (timers :initarg :timers :accessor timers :initform nil)
   (user-errors :initarg :user-errors :accessor user-errors :initform nil)))

(defmethod md-awaken :before ((self tk-object))
  (make-tk-instance self))

(define-symbol-macro .tkw (nearest self window))

;;; --- timers ----------------------------------------

(defun never-unchanged (new old) (declare (ignore new old)))

;;;
;;; Now, not one but three incredibly hairy gyrations Cells-wise:
;;;
;;;    - repeat cannot be ephemeral, but we want repeated (setf (^repeat) 20)'s each to fire,
;;;      so we specify an unchanged-if value that always "no", lying to get propagation
;;;
;;;    - the executions rule is true obfuscated code. It manages to reset the count to zero
;;;      on repeated (setf ... 20)'s because on the second repetition we know we will hit the rule
;;;      with repeat non-null (20, in fact) and the ephemeral executed will be nil (because it is
;;;      only non-nil during propagation of (setf (executed...) t). not for Cell noobs.
;;;
;;;    - holy toledo. The /rule/ for after-factory sends the after command to Tk itself! I could just
;;;      return a list of the delay and the callback and have an observer dispatch it, but it would
;;;      have to so so exactly as the rule does, by dropping it in the deferred client queue.
;;;      In a sense I am starting here to leverage Cells3 queues to simplify things. Mind you, if
;;;      Timer evolves to where we let the client write its own after factory, we might want to
;;;      factor out the actual dispatch into an observer to make it transparent (assuming that is
;;;      not why they are supplying their own after-factory.
;;;
;;; Timer is totally a work-in-progress with much development ahead.
;;;

(defmodel timer ()
  ((id :cell nil :initarg :id :accessor id :initform nil
     :documentation "Assigned by TCL after each AFTER issued. Use to cancel.")
   (tag :cell nil :initarg :tag :accessor tag :initform :anon
     :documentation "A debugging aid")
   (state :initarg :state :accessor state :initform (c-in :on)
     :documentation "Turn off to stop, regardless of REPEAT setting")
   (action :initform nil :initarg :action :accessor action
     :documentation "A function (to which the timer is passed) invoked by when the TCL AFTER executes")
   (delay :initform 0 :initarg :delay :accessor delay
     :documentation "Millisecond interval supplied as is to TCL AFTER")
   (repeat :initform (c-in nil) :initarg :repeat :accessor repeat :unchanged-if 'never-unchanged
     :documentation "t = run continuously, nil = pause, a number N = repeat N times")
   (executed :cell :ephemeral :initarg :executed :accessor executed :initform (c-in nil)
     :documentation "Internal: set after an execution")
   (executions :initarg :executions :accessor executions
     :documentation "Number of times timer has had its action run since the last change to  the repeat slot"
     :initform (c? (if (null (^repeat))
                       0 ;; ok, repeat is off, safe to reset the counter here
                     (if (^executed)
                         (1+ (or .cache 0)) ;; obviously (.cache is the prior value, and playing it safe in case unset)
                       0)))) ;; hunh? executed is ephemeral. we are here only if repeat is changed, so reset
   
   (after-factory
    :documentation "Pure implementation"
    :initform (c? (bwhen (rpt (when (eq (^state) :on)
                                (^repeat)))
                    (when (or (zerop (^executions)) (^executed)) ;; dispatch initially or after an execution
                      (when (if (numberp rpt)
                                (< (^executions) rpt)
                              rpt) ;; a little redundant since bwhen checks that rpt is not nil
                        (with-integrity (:client `(:fini ,self)) ;; just guessing as to when, not sure it matters
                          (setf (id self) (after (^delay) (lambda ()
                                                            (when (eq (^state) :on)
                                                              (funcall (^action) self)
                                                              (setf (^executed) t)))))))))))))


(defobserver timers ((self tk-object) new-value old-value)
  (dolist (k (set-difference old-value new-value))
    (setf (state k) :off)
    (when (id self)
      (after-cancel (id k))))) ;; Tk doc says OK if cancelling already executed

;;; --- widget -----------------------------------------


(defmodel widget (family tk-object)
  ((path :accessor path :initarg :path
     :initform (c? (trc nil "path calc" self (parent-path (fm-parent self)) (md-name self))
                 (format nil "~(~a.~a~)"
                     (parent-path (fm-parent self))
                     (md-name self))))
   (packing :reader packing :initarg :packing :initform nil)
   (gridding :reader gridding :initarg :gridding :initform nil)
   (enabled :reader enabled :initarg :enabled :initform t)
   (bindings :reader bindings :initarg :bindings :initform nil)
   (menus :reader menus :initarg :menus :initform nil
     :documentation "An assoc of an arbitrary key and the associated CLOS menu instances (not their tk ids)")
   (image-files :reader image-files :initarg :image-files :initform nil)
   (selector :reader selector :initarg :selector
     :initform (c? (upper self selector))))
  (:default-initargs
      :id (gentemp "W")))

(defun widget-menu (self key)
  (or (find key (^menus) :key 'md-name)
    (break "The only menus I see are~{ ~a,~} not requested ~a" (mapcar 'md-name (^menus)) key)))

(defmacro ^widget-menu (key)
  `(widget-menu self ,key))

(defmethod make-tk-instance ((self widget))
  (setf (gethash (^path) (dictionary .tkw)) self)
  (when (tk-class self)
    (tk-format `(:make-tk ,self) "~(~a~) ~a ~{~(~a~) ~a~^ ~}"
      (tk-class self) (path self)(tk-configurations self)) :stdfctry))

(defmethod tk-configure ((self widget) option value)
  (tk-format `(:configure ,self ,option) "~a configure ~(~a~) ~a" (path self) option (tk-send-value value)))

(defmethod not-to-be :after ((self widget))
  (trc nil "not-to-be tk-forgetting true widget" self)
  (tk-format `(:forget ,self) "pack forget ~a" (^path))
  (tk-format `(:destroy ,self) "destroy ~a" (^path)))

;;; --- bindings ------------------------------------------------------------

(defobserver bindings () ;;; (w widget) event fun)
  ;
  ; when we get dynamic with this cell we will have to do the kids
  ; thing and worry about extant new-values, de-bind lost old-values
  ;
  (with-integrity (:client `(:bind ,self))
    (dolist (bspec new-value)
      (if (eql (length bspec) 3) ;; getting wierd here
          (destructuring-bind (event fmt fn) bspec
            (let ((name (gentemp "BNDG")))
              (tk-format `(:bind ,self) "bind ~a ~a ~a" ;; {puts {:callback ~a}}"
                (^path) event (format nil fmt (register-callback self name fn)))))
        (destructuring-bind (event fn) bspec
          (bind (^path) event fn))))))

;;;  --- packing ---------------------------------------------------------

(defobserver packing ((self widget))
  (when new-value
    (assert (null (kids-packing .parent)) ()
      "Do not specify packing (here for ~a) unless parent leaves kids-packing unspecified. 
This parent is ~a, kids-packing ~a" self (list .parent (type-of .parent)) (kids-packing .parent)))
  ;
  ; This use next of the parent instead of self is pretty tricky. It has to do with getting
  ; the pack commands out nested widgets before parents. The pack command issued on behalf
  ; of a top frame is sorted on the parent. Now we have to pack the top frame. If we associate
  ; the command with the frame, the sort is a tie and either might go first. So we continue
  ; the theme and associate /this/ pack with this top frame's parent. Note that we cannot go the
  ; normal route and pack the kids in their own context, because multiple kids get packed
  ; in one pack statement (and we cannot arbitrarily pack with the first kid because this is a nested
  ; deal and any kid might have kids, so each family packs associated with itself)
  ;
  (when (and new-value (not (typep .parent 'panedwindow)))
    (tk-format `(:pack ,(fm-parent self)) new-value)))

(defmacro c?pack-self (&optional (modifier$ ""))
  `(c? (format nil "pack ~a ~a" (path self) ,modifier$)))

;;; --- grids -------------------------------------------------------------------------

(defmodel grid-manager ()())

(defobserver gridding ((self grid-manager))
  (when new-value
    (loop for k in (^kids)
          when (gridding k)
          do (tk-format `(:grid ,k) (format nil "grid ~a ~a" (path k) (gridding k))))
    (destructuring-bind (&key columns rows) new-value
      (when columns
        (loop for config in columns
              for idx upfrom 0
              do (tk-format `(:grid ,self) (format nil "grid columnconfigure ~a ~a ~a" (^path) idx config))))
      (when columns
        (loop for config in rows
              for idx upfrom 0
              do (tk-format `(:grid ,self) (format nil "grid rowconfigure ~a ~a ~a" (^path) idx config)))))))

;;; --- items -----------------------------------------------------------------------

(defmodel item (tk-object)
  ((id-no :cell nil :initarg :id-no :accessor id-no :initform nil)
   (coords :initarg :coords :initform nil))
  (:documentation "not full blown widgets, but decorations thereof")
  (:default-initargs
      :id (gentemp "I")))

(defmethod make-tk-instance ((self item))
  (when (tk-class self)
    (with-integrity (:client `(:make-tk ,self))
       (tk-format :grouped "senddata [~a create ~a ~{ ~a~}  ~{~(~a~) ~a~^ ~}]"
         (path .parent) (down$ (tk-class self)) (coords self) (tk-configurations self))
       (setf (id-no self) (read-data)))))

(defmethod tk-configure ((self item) option value)
  (assert (id-no self) () "cannot configure item ~a until instantiated and id obtained" self)
  (tk-format `(:itemconfigure ,self ,option)
    "~A itemconfigure ~a ~a {~a}" (path .parent) (id-no self) (down$ option) value))

(defobserver coords ()
  (when (and (id-no self) new-value)
    (tk-format `(:configure ,self) 
      "~a coords ~a ~{ ~a~}" (path .parent) (id-no self) new-value)))

(defmethod not-to-be :after ((self item))
  (trc nil "whacking item" self)
  (tk-format `(:delete ,self) "~a delete ~a" (path (upper self widget)) (id-no self)))

(defparameter *tk-changers* nil)

;;; --- deftk --------------------

(defmacro deftk (class superclasses
                         (&rest std-slots)
                         &rest defclass-options)
  (destructuring-bind (&optional tk-class &rest tk-options)
      (cdr (find :tk-spec defclass-options :key 'car))
    
    (setf tk-options (tk-options-normalize tk-options))
    
    (multiple-value-bind (slots outputs)
        (loop for (slot-name tk-option) in tk-options
            collecting `(,slot-name :initform nil
                          :initarg ,(intern (string slot-name) :keyword)
                          :accessor ,slot-name)
            into slot-defs
            when tk-option
            collecting `(defobserver ,slot-name ((self ,class))
                          (when old-value-boundp
                            (tk-configure self ,(string tk-option) (or new-value ""))))
            into outputs
            finally (return (values slot-defs outputs)))
      `(progn
         (defmodel ,class ,(or superclasses '(widget))
           (,@(append std-slots slots))
           ,@(remove-if (lambda (k) (find k '(:default-initargs :tk-spec))) defclass-options :key 'car)
           (:default-initargs
               ,@(when tk-class `(:tk-class ',tk-class))
             ,@(cdr (find :default-initargs defclass-options :key 'car))))
         (defmethod tk-class-options append ((self ,class))
           ',tk-options)
         (defmacro ,(intern (conc$ "MK-" (symbol-name class))) (&rest inits)
           `(make-instance ',',class
              :fm-parent *parent*
              ,@inits))
         ,@outputs))))

(defun tk-options-normalize (tk-options)
  "normalize '(-aaa (tk-bbb -bbb)) => '((aaa -aaa)(tk-bbb -bbb))"
  (loop for tk-option-def in tk-options
      for slot-name = (intern (de- (if (atom tk-option-def)
                                       tk-option-def (car tk-option-def))))
      collecting (list slot-name (if (atom tk-option-def)
                                     tk-option-def (cadr tk-option-def)))))

(eval-when (compile load eval)
  (defun de- (sym)
    (remove #\- (symbol-name sym) :end 1)))
  
(defgeneric tk-class-options (self)
  (:method-combination append))

(defun tk-configurations (self)
  (loop for (slot-name tk-option) in (remove-duplicates (tk-class-options self) :key 'second)
      for slot-value = (funcall slot-name self) ;; must go thru accessor with Cells, cannot (slot-value self slot-name)
      when (and tk-option slot-value)
      nconcing (list tk-option (tk-send-value slot-value))))

; --- callbacks ----------------------------------------------------


(defun tk-callback (self id-suffix fn &optional command)
  (declare (ignorable command))
  (let ((id (register-callback self id-suffix fn)))
    (trc nil "tk-callback" self id)
    (list 'callback id)))

(defun tk-callbackstring (self id-suffix tk-token fn)
  (format nil "callbackstring ~s ~a; return 1;"
    (register-callback self id-suffix fn)
    (string tk-token)))

(defun tk-callbackstring-x (self id-suffix tk-token fn)
  (format nil "callbackstring ~s ~a"
    (register-callback self id-suffix fn)
    (string tk-token)))

(defun tk-callbackval (self id-suffix fn &optional command)
  (declare (ignorable command))
  (format nil (or command "callbackval ~s")
    (register-callback self id-suffix fn)))

(defun register-callback (self callback-id fun)
  (assert callback-id)
  (let ((id (format nil "~a.~a" (path-index self) callback-id)))
    ;; (trc "registering callback" self :id (type-of id) id)
    (add-callback id fun)
    id))

(defmethod path-index (self) (^path))

(defun tk-eval-var (var)
  (tk-format :grouped "senddatastring [set ~a]" var)
  (read-data))

(defun tk-eval-list (self form$)
  (declare (ignore self))
  (tk-format :grouped "senddatastrings [~a]" form$)
  (read-data))

;--- selector ---------------------------------------------------

(defmodel selector () ;; mixin
  ((selection :initform nil :accessor selection :initarg :selection)
   (tk-variable :initform nil :accessor tk-variable :initarg :tk-variable
     :documentation "The TK node name to set as the selection changes (not the TK -variable option)"))
  (:default-initargs
      :selection (c-in nil)
      :tk-variable (c? (^path))))

(defobserver selection ()
  ;
  ; handling varies on this, so we hand off to standard GF lest the PROGN
  ; method combo on slot-listener cause multiple handling
  ;
  (tk-output-selection self new-value old-value old-value-boundp))

(defmethod tk-output-selection (self new-value old-value old-value-boundp)
  (declare (ignorable old-value old-value-boundp))
  (trc nil "selection output" self new-value)
  (when new-value
    (tk-format `(:variable ,self) "set ~(~a~) ~a" ;; was ~a at the end, but no good for a font name with embedded spaces
      (tk-variable self)
      (tk-send-value new-value))))

;;; --- images -------------------------------------------------------

(defobserver image-files ()
  ;
  ; I do not know how to create the photo for X before X exists
  ; though it seems to work. <g> perhaps Tk understands it does not need to
  ; place the image in a tree and lets the undefined path go? If so,
  ; just add :pre-make-kt before :make-kt in the sort list
  ;
  (loop for (name file-pathname) in (set-difference new-value old-value :key 'car) 
      do (tk-format `(:pre-make-tk  ,self) "image create photo ~(~a.~a~) -file ~a"
           (^path) name (tkescape (namestring file-pathname)))))


;;; --- menus ---------------------------------

(defun pop-up (menu x y)
  (tk-format-now "tk_popup ~A ~A ~A" (path menu) x y))