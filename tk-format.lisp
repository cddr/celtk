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

; --- tk-format --- talking to wish/Tk -----------------------------------------------------

(defun tk-user-queue-sort (task1 task2)
  "Intended for use as user queue sorter, to make Tk happy by giving it stuff in the order it needs to work properly."
  (let ((priority '(:destroy :pre-make-tk :make-tk :post-make-tk :variable :bind :selection :trace :configure :grid :pack :fini)))
    (destructuring-bind (type1 self1 &rest dbg) task1
      (declare (ignorable dbg))
      (assert type1)
      (assert (find type1 priority) () "unknown task type ~a in task ~a" type1 task1)
      (destructuring-bind (type2 self2 &rest dbg) task2
        (declare (ignorable dbg))
        (assert type2)
        (assert (find type2 priority) () "unknown task type ~a in task ~a" type2 task2)
        (let ((p1 (position type1 priority))
              (p2 (position type2 priority)))
          (cond
           ((< p1 p2) t)
           ((< p2 p1) nil)
           (t (case type1 ;; they are the same if we are here
                (:make-tk
                 (fm-ordered-p self1 self2))
                (:pack
                 (fm-ascendant-p self2 self1))))))))))


(defun tk-user-queue-handler (user-q)
  (loop for (nil #+not defer-info . task) in (prog1
                                                 (sort (fifo-data user-q) 'tk-user-queue-sort :key 'car)
                                               (fifo-clear user-q))
        do
        (trc nil "!!! --- tk-user-queue-handler dispatching" defer-info)
        (funcall task)))

#+nahh
(defun tk-format-now (fmt$ &rest fmt-args &aux (tk$ (apply 'format nil fmt$ fmt-args)))
  ;
  ; --- pure debug stuff ---
  ;
  (let ((yes '( "coords" )) ;; '("scroll" "pkg-sym"))
        (no  '()))
    (declare (ignorable yes no))
    (bwhen (st (search "\"Alt Q\"" tk$))
        (replace tk$ "{Alt Q}" :start1 st))
    (when (and (find-if (lambda (s) (search s tk$)) yes)
                      (not (find-if (lambda (s) (search s tk$)) no)))
      (format t "~&tk> ~A~%" #+nah cells::*data-pulse-id* tk$)
      #+nah (unless (find #\" tk$)
              (break "bad set ~a" tk$))))
  (assert (wish-stream *wish*)) ;; when not??
  ;
  ; --- serious stuff ---
  ;
  (format (wish-stream *wish*) "~A~%" tk$)
  (force-output (wish-stream *wish*)))


(defun tk-format-now (fmt$ &rest fmt-args &aux (tk$ (apply 'format nil fmt$ fmt-args)))
  ;;(format t "~&tk> ~A~%" tk$)
  (format (wish-stream *wish*) "~A~%" tk$)
  (force-output (wish-stream *wish*)))

(defun tk-format (defer-info fmt$ &rest fmt-args)
  "Format then send to wish (via user queue)"
  (assert (or (eq defer-info :grouped)
            (consp defer-info)) () "need defer-info to sort command ~a. Specify :grouped if caller is managing user-queue"
    (apply 'format nil fmt$ fmt-args))

  (when (eq defer-info :grouped)
    (setf defer-info nil))
  (flet ((do-it ()
           (apply 'tk-format-now fmt$ fmt-args)))
    (if defer-info
        (with-integrity (:client defer-info)
          (do-it))
    (do-it))))

(defmethod tk-send-value ((s string))
  (format nil "~s" #+not "{~a}" s))

(defmethod tk-send-value (other)
  (format nil "~a" other))

(defmethod tk-send-value ((s symbol))
  (down$ s))

(defmethod tk-send-value ((p package))
  (package-name p))

(defmethod tk-send-value ((values list))
  (format nil "{~{~a~^ ~}}" (mapcar 'tk-send-value values)))

(defmethod parent-path ((nada null)) "")
(defmethod parent-path ((self t)) (^path))

