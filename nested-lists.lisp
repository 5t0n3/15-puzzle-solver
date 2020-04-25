(in-package :com.stone.nested-lists)

(defun find-nested-index (nested-list item)
  "Finds the outer and inner indices of an element in a list of lists."
  (loop for row in nested-list
        for row-num = 0 then (1+ row-num)
        for item-index = (position item row)
        if item-index return (list item-index row-num)))

(defun nested-nth (index-list nested-list)
  "Returns the element at the specified indicies in a list of lists."
  (destructuring-bind (inner-idx outer-idx) index-list
    (nth inner-idx (nth outer-idx nested-list))))

;; TODO: Improve on the documentation for this function
(defun get-row (index-list nested-list)
  "Returns the sublist indise nested-list corresponding to the second index in index-list."
  (destructuring-bind (_ outer-idx) index-list
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

