(in-package :cl-user)

(defpackage :ten/tests
  (:use :cl :ten :fiveam)
  (:export :run-tests))

(in-package #:ten/tests)

(def-suite ten-tests)

(defun run-tests ()
  (debug! 'ten-tests))

(in-suite ten-tests)

(def-test inheritance-test ()
  (is (not (null (search "This is parent body" (ten/examples:parent)))))
  (is (not (null (search "This is child1 body" (ten/examples:child1)))))
  (is (null (search "This is parent body" (ten/examples:child1))))
  (is (not (null (search "This is child2 body" (ten/examples:child2)))))
  (is (not (null (search "This is parent body" (ten/examples:child2)))))
  (is (not (null (search "This is child3 body" (ten/examples:child3)))))
  (is (not (null (search "This is child2 body" (ten/examples:child3)))))
  (is (not (null (search "This is parent body" (ten/examples:child3))))))
