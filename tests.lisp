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

(def-test escaping-test ()
  (is (null (search "<div></div>" (ten/examples:escaping2 "<div></div>"))))
  (is (not (null (search "&lt;div&gt;&lt;/div&gt;" (ten/examples:escaping2 "<div></div>")))))
  (is (not (null (search "<div></div>" (ten/examples:escaping3 "<div></div>")))))
  (is (not (null (search "<div></div>" (ten/examples:escaping4 "<div></div>"))))))

(def-test if-expression-test ()
  ;; {% if %} without {% else %} should fail to compile
  (signals error 
    (ten:compile-template "{% template if-test-1 () (locale) %}
<html {% if locale %}lang=\"{{ locale }}\"{% end %}>
{% end %}"))
  ;; {% if %} with an {% else %} compiles
  (finishes
    (ten:compile-template "{% template if-test-2 () (locale) %}
<html {% if locale %}lang=\"{{ locale }}\"{% else %}{% end %}>
{% end %}"))
  ;; {% when %} does not need an else
  (finishes
    (ten:compile-template "{% template if-test-3 () (locale) %}
<html {% when locale %}lang=\"{{ locale }}\"{% end %}>
{% end %}")))
