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

(defun new-empty-location (state action)
  "Returns the new location of the :EMPTY space after taking an action on the given state."
  (destructuring-bind (empty-x empty-y) (empty-location state)
    (case action
      (:up (list empty-x (1+ empty-y)))
      (:down (list empty-x (- empty-y 1)))
      (:left (list (1+ empty-x) empty-y))
      (:right (list (- empty-x 1) empty-y)))))

(defun take-action (state action)
  "Executes an action on a given state and returns the resulting state, assuming it is legal."
  (let* ((new-empty-loc (new-empty-location state action))
         (moved-element (nested-nth new-empty-loc state)))
    (list :emptyloc new-empty-loc
          :state (if moved-element (swap-items-nested state :empty moved-element) nil))))

(defun action-legal-p (action-result)
  "Verifies that a given action results in a legal state."
  (let ((result-state (getf action-result :state)))
    (not (null result-state))))

(defun legal-actions (state)
  "Checks which actions are legal (i.e. don't move off the board) to take on the current state."
  (let ((legal-actions-list ()))
    (dolist (action *action-choices*)
     (let* ((action-result (take-action state action))
            (result-state (getf action-result :state)))
       (if (action-legal-p action-result)
           (push (list :action action :result result-state) legal-actions-list))))
    legal-actions-list))

(defun possible-actions (state frontier explored)
  "Returns a list of all of the possible actions that can be taken that haven't already."
  (let ((legal-actions (legal-actions state)))
    (format t "Possible actions: ~a" legal-actions)))
