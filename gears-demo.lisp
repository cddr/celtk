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