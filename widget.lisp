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

(in-package :Celtk)

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
   (event-handlers :reader event-handlers :initarg :event-handlers :initform (make-hash-table))
   (menus :reader menus :initarg :menus :initform nil
     :documentation "An assoc of an arbitrary key and the associated CLOS menu instances (not their tk ids)")
   (image-files :reader image-files :initarg :image-files :initform nil)
   (selector :reader selector :initarg :selector
     :initform (c? (upper self selector)))
   (on-event :initform nil :initarg :on-event :accessor on-event))
  (:default-initargs
      :id (gentemp "W")))

(defclass commander ()
  ()
  (:default-initargs
      :command (c? (format nil "call-back ~(~a~)" (^path)))))


(defun widget-menu (self key)
  (or (find key (^menus) :key 'md-name)
    (break "The only menus I see are~{ ~a,~} not requested ~a" (mapcar 'md-name (^menus)) key)))

(defmacro ^widget-menu (key)
  `(widget-menu self ,key))

(defmethod make-tk-instance ((self widget)) 
  (setf (gethash (^path) (dictionary .tkw)) self)
  (when (tk-class self)
    (tk-format `(:make-tk ,self) "~(~a~) ~a ~{~(~a~) ~a~^ ~}" ;; call to this GF now integrity-wrapped by caller
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
  ; /// think about trying again to wrap this whole thing in one (with-integrity '(:client...
  ; to avoid separate enqueues for each binding (but then to tk-format-now so one does not
  ; simpy enqueue again when dispatched.
  ;
  (dolist (bspec new-value)
    (destructuring-bind (event fn &optional event-info) bspec
        (bind self event fn event-info))))

;;; --- items -----------------------------------------------------------------------

(eval-when (compile load eval)
  (export '(canvas-offset ^canvas-offset coords-tweak ^coords-tweak caret-tweak ^caret-tweak
             decorations ^decorations)))

(defmodel item-geometer () ;; mix-in
  ((canvas-offset :initarg :canvas-offset :accessor canvas-offset
     :initform (c_? (eko (nil "standard canvas offset" self (type-of self) (^p-offset))
                     (c-offset self))))
   (caret-tweak :initarg :caret-tweak :accessor caret-tweak :initform '(0 0))
   (l-bounds :initarg :l-bounds :initform nil :reader l-bounds
     :documentation "Vector of local left, top, right, bottom")
   (p-offset :initarg :p-offset :reader p-offset :initform '(0 0))
   (p-bounds :initarg :p-bounds :reader p-bounds
     :documentation "Vector of parent-relative left, top, right, bottom"
     :initform (c_? (when (and (^l-bounds)(^p-offset) )
                     (bounds-offset (^l-bounds) (^p-offset))))))
  (:documentation "For things like mx-power, which inhabit canvases but need no item for visual representation."))

(defmethod l-bounds :around (i)
  (or (call-next-method)
    (break "no l-bounds for ~a" i)))

(defmethod anchor (other)(declare (ignore other)) nil)

(defmodel item (item-geometer tk-object)
  ((id-no :cell nil :initarg :id-no :accessor id-no :initform nil)
   (l-coords :initarg :l-coords :initform nil :accessor l-coords)
   (coords-tweak :initarg :coords-tweak :initform '(0 0) :accessor coords-tweak
     :documentation "Text items need this to get positioned according to baseline")
   (coords :initarg :coords :accessor coords
     :initform (c_? (eko (nil "final coords" self (anchor self)(^l-coords)(^canvas-offset)(^coords-tweak))
                     (loop for coord-xy = (^l-coords) then (cddr coord-xy)
                         while coord-xy
                         nconcing (mapcar '+ coord-xy (^canvas-offset) (^coords-tweak))))))
   (decorations :initarg :decorations :accessor decorations :initform nil
     :documentation "eg, For a left parens text item, the corresponding right parens text item")
   )
  (:documentation "Things you put on a canvas")
  (:default-initargs
      :id (gentemp "I")))

(defmethod make-tk-instance :around ((self item))
  (when (upper self canvas)
    (call-next-method)))

(defmethod make-tk-instance ((self item))
  (when (tk-class self)
    (with-integrity (:client `(:make-tk ,self))
      (ASSERT (^coords) () "Item ~a missing req'd coords" self)
      (setf (id-no self) (tk-eval "~a create ~a ~{ ~a~}  ~{~(~a~) ~a~^ ~}"
                           (path (upper self canvas))
                           (down$ (tk-class self))
                           (coords self)
                           (tk-configurations self))))))

(defmethod tk-configure ((self item) option value)
  (assert (id-no self) () "cannot configure item ~a until instantiated and id obtained" self)
  (tk-format `(:configure ,self ,option)
    "~A itemconfigure ~a ~a ~a" (path .parent) (id-no self) (down$ option) (tk-send-value value)))

(defobserver coords ()
  (when (and (id-no self) new-value)
    (trc nil "coords observer setting item" self (id-no self))
    (tk-format `(:configure ,self) 
      "~a coords ~a ~{ ~a~}" (path .parent) (id-no self) new-value)))

(defmethod not-to-be :before ((self item))
  (dolist (k (^decorations))
    (trc nil "dying item nails decoration" k)
    (not-to-be k)))

(defmethod not-to-be :after ((self item))
  (trc nil "whacking item" self)
  (tk-format `(:delete ,self) "~a delete ~a" (path (upper self widget)) (id-no self)))

(defobserver decorations ((self item) new-kids old-kids)
  ;; (trc "decorations" self new-kids old-kids)
  (dolist (k (set-difference old-kids new-kids))
    (trc "kids change nailing lost decoration" k)
    (not-to-be k)))

;;; --- widget mixins ------------------------------

;;; --- selector ---------------------------------------------------

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
  (trc "popping up" menu x y)
  (tk-format-now "tk_popup ~A ~A ~A" (path menu) x y))