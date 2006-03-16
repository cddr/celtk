(load (make-pathname :device "c"
        :directory '(:absolute "0dev" "cells")
        :name "asdf"
        :type "lisp"))

(push (make-pathname :device "c" :directory '(:absolute "0dev" "cells"))
    asdf:*central-registry*)

(push (make-pathname :device "c" :directory '(:absolute "0dev" "Celtk"))
  asdf:*central-registry*)

(ASDF:OOS 'ASDF:LOAD-OP :Celtk :force t)

#+gratuitousfeature
(ctk::tk-test)

