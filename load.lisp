;;;
;;; 
;;; First, grab these:
;;;
;;;    http://common-lisp.net/cgi-bin/viewcvs.cgi/cells/?root=cells
;;;    Celtk: http://common-lisp.net/cgi-bin/viewcvs.cgi/Celtk/?root=cells
;;;    CFFI: http://common-lisp.net/project/cffi/releases/cffi_0.9.1.tar.gz
;;;    cl-opengl: http://common-lisp.net/cgi-bin/darcsweb/darcsweb.cgi?r=cl-opencl%20cl-opengl;a=summary 
;;
;;; At the bottom of any of those pages look for a "Download tarball" link. Except cl-opengl, those guys
;;; are not download-friendly.
;;;
;;; Next, get ASDF loaded:

#+eval-this-if-you-do-not-autoload-asdf
(load (make-pathname #+lispworks :host #-lispworks :device "c"
        :directory '(:absolute "0dev" "cells")
        :name "asdf"
        :type "lisp"))

;;; /After/ you have manually evaluated the above form, you can tell ASDF
;;; where you put everything by adjusting these paths and evaluating:

(progn
  (push (make-pathname #+lispworks :host #-lispworks :device "c"
                       :directory '(:absolute "0dev" "cells"))
        asdf:*central-registry*)

  (push (make-pathname #+lispworks :host #-lispworks :device "c"
                       :directory '(:absolute "1-devtools" "cffi"))
        asdf:*central-registry*)

  (push (make-pathname #+lispworks :host #-lispworks :device "c"
                       :directory '(:absolute "1-devtools" "cl-opengl"))
        asdf:*central-registry*)

  (push (make-pathname #+lispworks :host #-lispworks :device "c"
                       :directory '(:absolute "0dev" "Celtk"))
        asdf:*central-registry*))

;;; and now you can try building the whole mess:

(ASDF:OOS 'ASDF:LOAD-OP :CELTK)

;;; and test:

(ctk::test-window 'celtk-user::lotsa-widgets)

;;; When that crashes, track down all the define-foreign-library calls in the source
;;; and fix the pathnames to point to your shared libraries.

