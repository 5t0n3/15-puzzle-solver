(in-package :com.stone.solver-utils)

(defclass puzzle-node ()
  ((current-state :initarg :state :reader state)
   (parent-node :initarg :parent :reader parent)
   (previous-action :initarg :action)
   (path-cost :initform 0 :initarg :cost :reader cost))
  (:documentation "A node representing a possible state of the 15 puzzle along with other information."))

(defparameter *goal-state* '((1 2 3 4)
                             (5 6 7 8)
                             (9 10 11 12)
                             (13 14 15 :empty)))

(defvar *action-choices* '(:up :down :left :right))

;; TODO: Maybe implement this as a function? It's more of an action thing than a node thing
(defgeneric move-score (state)
  (:documentation "Scores a node based on various criteria, such as Manhattan distance or cost."))

(defun manhattan-distance (current-pos goal-pos)
  "Returns the Manhattan distance between two points."
  (destructuring-bind (cx cy gx gy) (append current-pos goal-pos)
    (+ (abs (- gx cx)) (abs (- gy cy)))))

(defun empty-location (state)
  "Returns the location of the :EMPTY space in the puzzle."
  (find-nested-index state :empty))

(defun take-action (state action)
  "Executes an action on a given state and returns the resulting state, assuming it is legal."
  (destructuring-bind (empty-x empty-y) (empty-location state)
    (let* ((new-empty-loc (case action
                           (:up (list empty-x (1+ empty-y)))
                           (:down (list empty-x (- empty-y 1)))
                           (:left (list (1+ empty-x) empty-y))
                           (:right (list (- empty-x 1) empty-y))))
           (moved-element (nested-nth new-empty-loc state))
           (state-copy (copy-list state)))
      (format t "Moved element: ~a" moved-element)
      new-empty-loc)))

;; TODO: Perhaps integrate this into the take-action function?
(defun action-legal-p (action state)
  "Verifies that a given action is legal in a given state."
  (destructuring-bind (result-x result-y) (take-action state action)
    (and (<= result-x 3) (<= result-y 3))))

(defun possible-actions (state frontier explored)
  "Returns a list of all of the possible actions that can be taken that haven't already."
  (let ((legal-actions ()))
    ;; Check if the actions are legal without checking for context
    (dolist (action *action-choices*)
      (if (action-legal-p action state) (push action legal-actions)))
    (format t "Possible actions: ~a" legal-actions)))
