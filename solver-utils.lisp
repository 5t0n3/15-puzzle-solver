(in-package :com.stone.solver-utils)

;; TODO: Implement error handling of illegal moves rather than just returning nil as the state

(defclass node-metadata ()
  ((previous-action :initarg :action :reader previous-action)
   (cost :initform 0 :initarg :cost :reader cost)
   (moved-tile :initarg :tile :reader moved-tile))
  (:documentation "Represents some metadata about a node."))

(defclass puzzle-node ()
  ((current-state :initarg :state :reader state)
   (parent-node :initarg :parent :reader parent)
   (metadata :initarg :metadata :reader metadata))
  (:documentation "A node representing a possible state of the 15 puzzle along with other information."))

(defparameter *goal-state* '((1 2 3 4)
                             (5 6 7 8)
                             (9 10 11 12)
                             (13 14 15 :empty)))

(defvar *action-choices* '(:up :down :left :right))

(defun node-print-state (node)
  "Prints the current state of a node as a 4x4 grid."
  (format t "~{~{~a~6,6t~}~%~}" (state node)))

;; TODO: Consider using something possibly more efficient than nested destructuring-binds
(defun manhattan-distance (first-point second-point)
  "Returns the Manhattan distance between two points."
  (destructuring-bind (x1 y1) first-point
    (destructuring-bind (x2 y2) second-point
      (+ (abs (- x2 x1)) (abs (- y2 y1))))))

(defun tile-goal-distance (tile state)
  "Returns the Manhattan distance between a tile's current and goal positions."
  (let ((goal-position (find-nested-index *goal-state* tile))
        (current-position (find-nested-index state tile)))
    (manhattan-distance current-position goal-position)))

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

(defun action-transform-state (state action)
  "Executes the given action on the state and returns the result along with some metadata."
  (handler-bind ((negative-index-error #'(lambda (c)
                                           (declare (ignore c))
                                           (invoke-restart 'return-nil))))
    (let* ((new-empty-loc (new-empty-location state action))
           (moved-tile (nested-nth new-empty-loc state)))
      (list :moved-tile moved-tile
            :state (if moved-tile
                       (swap-items-nested state :empty moved-tile)
                       nil)))))

;; TODO: This might be better implemented as a method
(defun update-metadata (action action-result parent-node)
  "Returns the updated metadata of a node after executing an action on it."
  (let ((parent-metadata (metadata parent-node))
        (moved-tile (getf action-result :moved-tile)))
    (make-instance 'node-metadata :tile moved-tile :cost (1+ (cost parent-metadata)) :action action)))

;; TODO: Handle an illegal (nil) state instead of letting it pass
(defun node-take-action (node action)
  "Executes the action on the provided node, creating a new node in the process."
  (let* ((current-state (state node))
         (action-result (action-transform-state current-state action))
         (new-metadata (update-metadata action action-result node)))
    (make-instance 'puzzle-node :state (getf action-result :state)
                                :metadata new-metadata
                                :parent node)))

;; TODO: Try writing action-legal-p without relying on take-action (I'm not sure if this is possible)
(defun action-legal-p (action-result)
  "Verifies that a given action results in a legal state."
  (let ((result-state (getf action-result :state)))
    (not (null result-state))))

;; TODO: This could probably be implemented with a plain loop rather than a dolist loop
(defun legal-actions (state)
  "Checks which actions are legal (i.e. don't move off the board) to take on the current state."
  (let ((legal-actions-list ()))
    (dolist (action *action-choices*)
      (let* ((action-result (action-transform-state state action))
             (result-state (getf action-result :state))
             (moved-tile (getf action-result :moved-tile)))
        (if (action-legal-p action-result)
            (push (list :action action :result result-state :tile moved-tile) legal-actions-list))))
    legal-actions-list))

(defun possible-actions (state frontier explored)
  "Returns a list of all of the possible actions that can be taken that haven't already."
  (let ((legal-action-metadata (legal-actions state))
        (frontier-states (mapcar #'state frontier))
        (explored-states (mapcar #'state explored)))
    (format t "Possible actions: ~a" legal-action-metadata)))

;; TODO: Once the frontier/explored sets are implemented, add them as parameters to this
;; and check for membership
(defun node-legal-actions (node)
  (let ((current-state (state node)))
    (legal-actions current-state)))

;; TODO: Implement a way to sort the actions on a state based on their associated cost

;; TODO: Check if all of the actions are legal and decide what to do if they aren't
(defun take-actions-list (state actions)
  "Takes the list of actions on the given state. Short-circuits to nil if any actions are illegal."
  (loop for action in actions
        for result = (take-action (or current-state state) action)
        for current-state = (getf result :state)
        unless (action-legal-p result) do (return nil)
          finally (return current-state)))

;; TODO: In the way of solving the puzzle, take all legal actions and compare them via scores
