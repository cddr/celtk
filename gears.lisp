
(in-package :celtk)

(in-package :celtk)

(defparameter *startx* nil)
(defparameter *starty* nil)
(defparameter *xangle0* nil)
(defparameter *yangle0* nil)
(defparameter *xangle* 0.0)
(defparameter *yangle* 0.0)

(defparameter *vTime* 100)

(defun gears () ;; ACL project manager needs a zero-argument function, in project package
  (test-window 'gears-demo))


(defmodel gears-demo (window)
  ((gear-ct :initform (c-in 1) :accessor gear-ct :initarg :gear-ct)
   (scale :initform (c-in 1) :accessor scale :initarg :scale))
  (:default-initargs
      :title$ "Rotating Gear Widget Test"
    :kids (c? (the-kids
               (mk-stack (:packing (c?pack-self))
                 (mk-label :text "Click and drag to rotate image")
                 #+tki (mk-row ()
                         (mk-button-ex ("  Add " (incf (gear-ct .tkw))))
                         (mk-button-ex ("Remove" (when (plusp (gear-ct .tkw))
                                                   (decf (gear-ct .tkw)))))
                         (mk-entry :id :vtime
                           :md-value (c-in "100"))
                         (mk-button-ex (" Quit " (progn))))
                 (make-instance 'gears
                   :fm-parent *parent*
                   :width 400
                   :height 400
                   :timer-interval nil #+tki (c? (or .cache ;; comment out just ".cache" for some fun
                                                 (eko ("vtime is")
                                                   (md-value (fm-other :vtime)))))
                   :double "yes"
                   :bindings nil #+wait (c? (list
                                             (list "<Button-1>"
                                               (lambda (event) 
                                                 (RotStart self
                                                   (event-root-x event)
                                                   (event-root-y event))))
                                             (list "<B1-Motion>"
                                               (lambda (event) 
                                                 (RotMove self
                                                   (event-root-x event)
                                                   (event-root-y event))) )))))))))

(defun RotStart (self x y)
  (setf *startx* x)
  (setf *starty* y)
  (let ((vPos (tk-eval-list "~a position" (^path)))) ;; this fails for me -- command not recognized, it seems
    (trc "got vpos" vpos)
    (setf *xangle0* (read-from-string (nth 0 vpos)))
    (setf *yangle0* (read-from-string (nth 1 vpos)))))

(defun RotMove (self x y)
  (setf *xangle* (+ *xangle0* (- x *startx*)))
  (setf *yangle* (+ *yangle0* (- y *starty*)))
  (tk-format-now "~a rotate ~a ~a" (^path) *xangle* *yangle*))
(defconstant +pif+ (coerce pi 'single-float))

(defmodel gears (togl)
  ((view-rotx :initform (c-in 20.0) :accessor view-rotx :initarg :view-rotx)
   (view-roty :initform (c-in 30.0) :accessor view-roty :initarg :view-roty)
   (view-rotz :initform (c-in 0.0) :accessor view-rotz :initarg :view-rotz)
   (gear1 :accessor gear1 :initform (c-in nil))
   (gear2 :accessor gear2 :initform (c-in nil))
   (gear3 :accessor gear3 :initform (c-in nil))
   (angle :initform (c-in 0.0) :accessor angle :initarg :angle)
   (frame-count :cell nil :initform 0 :accessor frame-count)
   (t0 :cell nil :initform 0 :accessor t0)
   ;
   (width :initarg :wdith :initform 400 :accessor width)
   (height :initarg :wdith :initform 400 :accessor height)))

(defmethod togl-timer-using-class ((self gears))
  (trc nil "enter gear timer" self (togl-ptr self) (get-internal-real-time))
  (incf (^angle) 2.0)
  (Togl_PostRedisplay (togl-ptr self)))

(defmethod togl-reshape-using-class ((self gears) width height)
  (trc "enter gear reshape" self width :height (type-of height) :voila height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (let ((h (/ height width)))
    (gl:frustum -1 1 (- h) h 5 60))
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (gl:translate 0 0 -40))

(defmethod togl-display-using-class ((self gears) &aux (scale (scale (upper self gears-demo))))
  (declare (ignorable scale))
  (with-slots (view-rotx view-roty view-rotz angle gear1 gear2 gear3)
      self
    (trc nil "in gear display" self (togl-ptr self)gear1 gear2 gear3 scale)
    (gl:clear-color 0 0 0 1)
    (gl:clear :color-buffer-bit :depth-buffer-bit)
    
    (gl:with-pushed-matrix
      (gl:rotate (incf view-rotx) 1 0 0)
      (gl:rotate view-roty 0 1 0)
      (gl:rotate view-rotz 0 0 1)
      
      (gl:with-pushed-matrix ; gear1
        (gl:translate -3 -2 0)
        (gl:rotate angle 0 0 1)
        (gl:call-list gear1)) 

      (gl:with-pushed-matrix ; gear2
        (gl:translate 3.1 -2 0)
        (gl:rotate (- (* -2 angle) 9) 0 0 1)
        (gl:call-list gear2))

      (gl:with-pushed-matrix ; gear3
        (gl:translate -3.1 4.2 0.0)
        (gl:rotate (- (* -2 angle) 25) 0 0 1)
        (gl:call-list gear3)))
    
    (Togl_SwapBuffers (togl-ptr self))
    
    (print-frame-rate self)))

(defun print-frame-rate (window)
  (with-slots (frame-count t0) window
    (incf frame-count)
    (let ((time (get-internal-real-time)))
      (when (= t0 0)
        (setq t0 time))
      (when (>= (- time t0) (* 1 internal-time-units-per-second))
        (let* ((seconds (/ (- time t0) internal-time-units-per-second))
               (fps (/ frame-count seconds)))
          (format *terminal-io* "~D frames in ~3,1F seconds = ~6,3F FPS~%"
                  frame-count seconds fps))
        (setq t0 time)
        (setq frame-count 0)))))

(defmethod togl-create-using-class ((self gears))
  (gl:light :light0 :position #(5.0 5.0 10.0 0.0))
  (gl:enable :cull-face :lighting :light0 :depth-test)

  ;; gear 1
  (setf (^gear1) (gl:gen-lists 1))
  (gl:with-new-list ((^gear1) :compile)
    (gl:material :front :ambient-and-diffuse #(0.8 0.1 0.0 1.0)) ; red
    (draw-gear 1.0 4.0 1.0 20 0.7))

  ;; gear 2
  (setf (^gear2) (gl:gen-lists 1))
  (gl:with-new-list ((^gear2) :compile)
    (gl:material :front :ambient-and-diffuse #(0.0 0.8 0.2 1.0)) ; green
    (draw-gear 0.5 2.0 2.0 10 0.7))
  ;; gear 3
  (setf (^gear3) (gl:gen-lists 1))
  (gl:with-new-list ((^gear3) :compile)
    (gl:material :front :ambient-and-diffuse #(0.2 0.2 1.0 1.0)) ; blue
    (draw-gear 1.3 2.0 0.5 10 0.7))
  (gl:enable :normalize))

(defun draw-gear (inner-radius outer-radius width n-teeth tooth-depth)
  "Draw a gear."
  (declare (single-float inner-radius outer-radius width tooth-depth)
           (fixnum n-teeth))
  (let ((r0 inner-radius)
        (r1 (- outer-radius (/ tooth-depth 2.0)))
        (r2 (+ outer-radius (/ tooth-depth 2.0)))
        (da (/ (* 2.0 +pif+) n-teeth 4.0)))
    (gl:shade-model :flat)
    (gl:normal 0 0 1)
    ;; Draw front face.
    (gl:with-primitives :quad-strip 
      (dotimes (i (1+ n-teeth))
        (let ((angle (/ (* i 2.0 +pif+) n-teeth)))
          (gl:vertex (* r0 (cos angle)) (* r0 (sin angle)) (* width 0.5))
          (gl:vertex (* r1 (cos angle)) (* r1 (sin angle)) (* width 0.5))
          (gl:vertex (* r0 (cos angle)) (* r0 (sin angle)) (* width 0.5))
          (gl:vertex (* r1 (cos (+ angle (* 3 da))))
                     (* r1 (sin (+ angle (* 3 da))))
                     (* width 0.5)))))
    ;; Draw front sides of teeth.
    (gl:with-primitives :quads
      (dotimes (i n-teeth)
        (let ((angle (/ (* i 2.0 +pif+) n-teeth)))
          (gl:vertex (* r1 (cos angle)) (* r1 (sin angle)) (* width 0.5))
          (gl:vertex (* r2 (cos (+ angle da))) (* r2 (sin (+ angle da)))
                     (* width 0.5))
          (gl:vertex (* r2 (cos (+ angle (* 2 da))))
                     (* r2 (sin (+ angle (* 2 da))))
                     (* width 0.5))
          (gl:vertex (* r1 (cos (+ angle (* 3 da))))
                     (* r1 (sin (+ angle (* 3 da))))
                     (* width 0.5)))))
    (gl:normal 0 0 -1)
    ;; Draw back face.
    (gl:with-primitives :quad-strip
      (dotimes (i (1+ n-teeth))
        (let ((angle (/ (* i 2.0 +pif+) n-teeth)))
          (gl:vertex (* r1 (cos angle)) (* r1 (sin angle)) (* width -0.5))
          (gl:vertex (* r0 (cos angle)) (* r0 (sin angle)) (* width -0.5))
          (gl:vertex (* r1 (cos (+ angle (* 3 da))))
                       (* r1 (sin (+ angle (* 3 da))))
                       (* width -0.5))
          (gl:vertex (* r0 (cos angle)) (* r0 (sin angle)) (* width -0.5))))) 
    ;; Draw back sides of teeth.
    (gl:with-primitives :quads
      (dotimes (i n-teeth)
        (let ((angle (/ (* i 2.0 +pif+) n-teeth)))
          (gl:vertex (* r1 (cos (+ angle (* 3 da))))
                     (* r1 (sin (+ angle (* 3 da))))
                     (* (- width) 0.5))
          (gl:vertex (* r2 (cos (+ angle (* 2 da))))
                     (* r2 (sin (+ angle (* 2 da))))
                     (* (- width) 0.5))
          (gl:vertex (* r2 (cos (+ angle da))) (* r2 (sin (+ angle da)))
                     (* (- width) 0.5))
          (gl:vertex (* r1 (cos angle)) (* r1 (sin angle)) (* (- width) 0.5)))))
    ;; Draw outward faces of teeth.
    (gl:with-primitives :quad-strip
      (dotimes (i n-teeth)
        (let ((angle (/ (* i 2.0 +pif+) n-teeth)))
          (gl:vertex (* r1 (cos angle)) (* r1 (sin angle)) (* width 0.5))
          (gl:vertex (* r1 (cos angle)) (* r1 (sin angle)) (* (- width) 0.5))
          (let* ((u (- (* r2 (cos (+ angle da))) (* r1 (cos angle))))
                 (v (- (* r2 (sin (+ angle da))) (* r1 (sin angle))))
                 (len (sqrt (+ (* u u) (* v v)))))
            (setq u (/ u len))
            (setq v (/ u len))
            (gl:normal v (- u) 0.0)
            (gl:vertex (* r2 (cos (+ angle da))) (* r2 (sin (+ angle da)))
                       (* width 0.5))
            (gl:vertex (* r2 (cos (+ angle da))) (* r2 (sin (+ angle da)))
                       (* (- width) 0.5))
            (gl:normal (cos angle) (sin angle) 0.0)
            (gl:vertex (* r2 (cos (+ angle (* 2 da))))
                       (* r2 (sin (+ angle (* 2 da))))
                       (* width 0.5))
            (gl:vertex (* r2 (cos (+ angle (* 2 da))))
                       (* r2 (sin (+ angle (* 2 da))))
                       (* (- width) 0.5))
            (setq u (- (* r1 (cos (+ angle (* 3 da))))
                       (* r2 (cos (+ angle (* 2 da))))))
            (setq v (- (* r1 (sin (+ angle (* 3 da))))
                       (* r2 (sin (+ angle (* 2 da))))))
            (gl:normal v (- u) 0.0)
            (gl:vertex (* r1 (cos (+ angle (* 3 da))))
                       (* r1 (sin (+ angle (* 3 da))))
                       (* width 0.5))
            (gl:vertex (* r1 (cos (+ angle (* 3 da))))
                       (* r1 (sin (+ angle (* 3 da))))
                       (* (- width) 0.5))
            (gl:normal (cos angle) (sin angle) 0.0))))
      (gl:vertex (* r1 (cos 0)) (* r1 (sin 0)) (* width 0.5))
      (gl:vertex (* r1 (cos 0)) (* r1 (sin 0)) (* (- width) 0.5)))
    ;; Draw inside radius cylinder.
    (gl:shade-model :smooth)
    (gl:with-primitives :quad-strip
      (dotimes (i (1+ n-teeth))
        (let ((angle (/ (* i 2.0 +pif+) n-teeth)))
          (gl:normal (- (cos angle)) (- (sin angle)) 0.0)
          (gl:vertex (* r0 (cos angle)) (* r0 (sin angle)) (* (- width) 0.5))
          (gl:vertex (* r0 (cos angle)) (* r0 (sin angle)) (* width 0.5)))))))