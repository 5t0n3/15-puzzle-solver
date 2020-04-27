(in-package :common-lisp-user)

(defpackage :com.stone.nested-lists
  (:use :common-lisp)
  (:export :nested-nth
           :find-nested-index
           :swap-items-nested
           :negative-index-error
           :return-nil))

(defpackage :com.stone.solver-utils
  (:use :common-lisp :com.stone.nested-lists)
  (:export :*goal-state*))

;; Load the files in the proper order
(load "nested-lists.lisp")
(load "solver-utils.lisp")
