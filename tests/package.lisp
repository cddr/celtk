(in-package :cl-user)

(defpackage :celtk-tests
  (:use :cl :celtk :5am)
  (:export :run-tests))

(in-package :celtk-tests)

(def-suite celtk-test-suite
    :description "The Celtk Test Suite")
