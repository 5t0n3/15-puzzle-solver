(in-package :common-lisp-user)

(defpackage :com.stone.nested-lists
  (:use :common-lisp)
  (:export :nested-nth
           :find-nested-index))

(defpackage :com.stone.solver-utils
  (:use :common-lisp :com.stone.nested-lists)
  (:export :*goal-state*))


