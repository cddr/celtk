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
  (:use :common-lisp :utils-kt :cells :cffi)
  (:export
   #:<1>
   #:title$ #:pop-up #:event-root-x #:event-root-y
   #:window #:panedwindow #:mk-row #:c?pack-self #:mk-stack #:mk-text-widget #:text-widget
   #:mk-panedwindow
   #:mk-stack #:mk-radiobutton #:mk-radiobutton-ex #:mk-radiobutton #:mk-label
   #:^selection #:selection #:selector
   #:mk-checkbutton #:button #:mk-button #:mk-button-ex  #:entry #:mk-entry #:text
   #:frame-stack #:mk-frame-stack #:path #:^path
   #:mk-menu-entry-radiobutton #:mk-menu-entry-checkbutton
   #:mk-menu-radio-group #:mk-menu-entry-separator
   #:mk-menu-entry-command #:mk-menu-entry-command-ex
   #:menu #:mk-menu #:^menus #:mk-menu-entry-cascade #:mk-menubar
   #:^entry-values #:tk-eval #:tk-eval-list #:scale #:mk-scale #:mk-popup-menubutton
   #:item #:polygon #:mk-polygon #:oval #:mk-oval #:line #:mk-line #:arc #:mk-arc
   #:text-item #:mk-text-item #:item-geometer
   #:rectangle #:mk-rectangle #:bitmap #:mk-bitmap #:canvas #:mk-canvas #:mk-frame-row
   #:mk-scrolled-list #:listbox-item #:mk-spinbox
   #:mk-scroller #:mk-menu-entry-cascade-ex
   #:with-ltk #:tk-format #:send-wish #:value #:.tkw
   #:tk-user-queue-handler #:user-errors #:^user-errors
   #:timer #:timers #:repeat #:executions #:state #:timer-reset #:make-timer-steps
   #:^widget-menu #:widget-menu #:tk-format-now
   #:coords #:^coords #:tk-translate-keysym
   #:do-on-event #:*tkw*))

(defpackage :celtk-user
  (:use :common-lisp :utils-kt :cells :celtk))

(in-package :Celtk)

(defvar *tki* nil)

(defparameter *tk-last* nil "Debug aid. Last recorded command send to Tk")

(defparameter *tkw* nil)

(define-symbol-macro .tkw (nearest self window))

; --- tk-format --- talking to wish/Tk -----------------------------------------------------

(defun tk-user-queue-sort (task1 task2)
  "Intended for use as user queue sorter, to make Tk happy by giving it stuff in the order it needs to work properly."
  (let ((priority '(:delete :forget :destroy 
                     :pre-make-tk :make-tk :make-tk-menubutton :post-make-tk 
                     :variable :bind :selection :trace :configure :grid :pack :fini)))
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
  #+shh (loop for (defer-info . nil) in (sort (copy-list (fifo-data user-q)) 'tk-user-queue-sort :key 'car)
        do (trc "user-q-handler sees" defer-info))
  (loop for (nil #+not defer-info . task) in (prog1
                                                 (sort (fifo-data user-q) 'tk-user-queue-sort :key 'car)
                                               (fifo-clear user-q))
        do
        (trc nil "!!! --- tk-user-queue-handler dispatching" defer-info)
        (funcall task)))

(defun replace-char (txt char with)
  (let ((pos (search char txt)))
    (loop
       while pos
       do
         (progn
           ;;(dbg "txt: ~a -> " txt)
           (setf txt (concatenate 'string (subseq txt 0 pos) with (subseq txt (1+ pos))))
           ;;(dbg " ~a~&" txt)
           (setf pos (search char txt :start2 (+ pos (length with)))))))
  txt)

(defun tkescape (txt)
  (setf txt (format nil "~a" txt))
  (replace-char
   (replace-char
    (replace-char
     (replace-char
      (replace-char
       txt "\\" "\\\\")
      "$" "\\$")
     "[" "\\[")
    "]" "\\]")
   "\"" "\\\""))

(defun tk-format-now (fmt$ &rest fmt-args &aux (tk$ (apply 'format nil fmt$ fmt-args)))
  ;
  ; --- debug stuff ---
  ;
  (let ((yes '("bind" "entry"))
        (no  '("tk-events")))

    (declare (ignorable yes no))
    (bwhen (st (search "\"Alt Q\"" tk$))
      (break "Hey, fix this.")
      (replace tk$ "{Alt Q}" :start1 st))

    (when (and (or (null yes) (find-if (lambda (s) (search s tk$)) yes))
            (not (find-if (lambda (s) (search s tk$)) no)))
      (format t "~&tk> ~a~%" tk$)))
  
  (assert *tki*)
  ;
  ; --- serious stuff ---
  ;
  (setf *tk-last* tk$)
  (tcl-eval-ex *tki* tk$))

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
  (if nil #+not (find #\\ s) ;; welllll, we cannot send: -text "[" to Tk because t misinterprets it, so we have to send the octal
                                                       ; which begins with \. There is probably a better way ///
      (format nil "\"~a\"" s) ;; no good if \ is in file path as opposed to escaping
      (format nil "~s" s)                                  ; this fails where I want to send a /Tk/ escape sequence "\065" 
                                                       ; because the ~s directive adds its own escaping
  ;;(format nil "{~a}" s)                                ;this fails, too, not sure why
  
  ))

(defmethod tk-send-value ((c character))
  ;
  ; all this just to display "[". Unsolved is how we will
  ; send a text label with a string /containing/ the character #\[
  ;
  (trc "tk-send-value" c (char-code c) (format nil "\"\\~3,'0o\"" (char-code c)))
  (format nil "\"\\~3,'0o\"" (char-code c)))

(defmethod tk-send-value (other)
  (format nil "~a" other))

(defmethod tk-send-value ((s symbol))
  (down$ s))

(defmethod tk-send-value ((p package))
  (package-name p))

(defmethod tk-send-value ((values list))
  (format nil "{~{~a~^ ~}}" (mapcar 'tk-send-value values)))

(defmethod parent-path ((nada null)) "")
(defmethod parent-path ((self t)) (path self))

; --- tk eval  ----------------------------------------------------



(defmethod path-index (self) (path self))

(defun tk-eval (tk-form$ &rest fmt-args
                 &aux (tk$ (apply 'format nil tk-form$ fmt-args)))
  (assert *tki* () "Global *tki* is not bound to anything, let alone a Tcl interpreter")
  (tk-format :grouped tk$)
  (tcl-get-string-result *tki*))

(defun tk-eval-var (var)
  (tk-eval "set ~a" var))

(defun tk-eval-list (tk-form$ &rest fmt-args)
  (tk-format :grouped (apply 'format nil tk-form$ fmt-args))
  (parse-tcl-list-result (tcl-get-string-result *tki*)))

(defun parse-tcl-list-result (result &aux item items)
  (when (plusp (length result))
    (trc nil "parse-tcl-list-result" result)
    (labels ((is-spaces (s)
               (every (lambda (c) (eql c #\space)) s))
             (gather-item ()
               (unless (is-spaces item)
                 ;(trc "item chars" (reverse item))
                 ;(trc "item string" (coerce (reverse item) 'string))
                 (push (coerce (nreverse item) 'string) items)
                 (setf item nil))))
      (loop with inside-braces
          for ch across result
          if (eql ch #\{)
          do (if inside-braces
                 (break "whoa, nested braces: ~a" result)
               (setf inside-braces t))
          else if (eql ch #\})
          do (setf inside-braces nil)
            (gather-item)
            (setf item nil)
          else if (eql ch #\space)
          if inside-braces do (push ch item)
          else do (gather-item)
            (setf item nil)
          else do (push ch item)
          finally (return (nreverse items))))))
        