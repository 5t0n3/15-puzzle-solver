(in-package :com.stone.nested-lists)

(define-condition negative-index-error (error) ()
  (:documentation "Represents an attempt to access a negative index in a list."))

(defun find-nested-index (nested-list item)
  "Finds the outer and inner indices of an element in a list of lists."
  (loop for row in nested-list
        for row-num = 0 then (1+ row-num)
        for item-index = (position item row)
        if item-index return (list item-index row-num)))

(defmacro negative-index-restarts (index-list callback-fn &rest callback-args)
  `(restart-case (error 'negative-index-error)
    (return-nil () nil)
    (make-positive () (funcall ,callback-fn (mapcar #'abs ,index-list) ,@callback-args))))

(defun nested-nth (index-list nested-list)
  "Returns the element at the specified indices in a list of lists. Errors on a negative index."
  (destructuring-bind (inner-idx outer-idx) index-list
    (cond
      ((some #'minusp index-list) (negative-index-restarts index-list 'nested-nth nested-list))
      (t (nth inner-idx (nth outer-idx nested-list))))))

;; TODO: Improve on the documentation for this function
(defun get-row (index-list nested-list)
  "Returns the sublist indise nested-list corresponding to the second index in index-list."
  (let ((outer-idx (nth 1 index-list)))
    (nth outer-idx nested-list)))

(defun (setf nested-nth) (new-value index-list nested-list)
  (let ((replace-row (get-row index-list nested-list)))
    (setf (nth (car index-list) replace-row) new-value)))

(defun swap-items-nested (original-list first-item second-item)
  "Non-destructively swaps the two given items in original-list, which must be a list of lists."
  (let ((first-idx (find-nested-index original-list first-item))
        (second-idx (find-nested-index original-list second-item))
        (copied-list (copy-tree original-list))) ; Note that copy-tree is used to avoid shared structure
    (setf (nested-nth first-idx copied-list) second-item)
    (setf (nested-nth second-idx copied-list) first-item)
    copied-list))
