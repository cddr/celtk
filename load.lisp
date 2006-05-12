#+eval-this-if-you-do-not-autoload-asdf
(load (make-pathname #+lispworks :host #-lispworks :device "c"
        :directory '(:absolute "0dev" "cells")
        :name "asdf"
        :type "lisp"))

(progn
  (push (make-pathname #+lispworks :host #-lispworks :device "c"
                       :directory '(:absolute "0dev" "cells"))
        asdf:*central-registry*)

  (push (make-pathname #+lispworks :host #-lispworks :device "c"
                       :directory '(:absolute "0devtools" "cffi"))
        asdf:*central-registry*)

  (push (make-pathname #+lispworks :host #-lispworks :device "c"
                       :directory '(:absolute "0devtools" "cl-opengl"))
        asdf:*central-registry*)

  (push (make-pathname #+lispworks :host #-lispworks :device "c"
                       :directory '(:absolute "0dev" "Celtk"))
        asdf:*central-registry*))

#-runtestsuite
(ASDF:OOS 'ASDF:LOAD-OP :CELLS)

#+runtestsuite
(ASDF:OOS 'ASDF:LOAD-OP :CELLS-TEST)

(ASDF:OOS 'ASDF:LOAD-OP :CELTK)

#+ortestceltk
(ctk:test-window 'celtk-user::ltktest-cells-inside)

#+opengl
(celtk-user::gears)