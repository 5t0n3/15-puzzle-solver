(in-package :com.stone.solver-utils)

;; TODO: Implement error handling of illegal moves rather than just returning nil as the state

;; TODO: Consider renaming cost slot to something more descriptive
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

;; TODO: Improve the documentation for this function
(defgeneric node-tile-score (node tile)
  (:documentation "Returns a rating of the cost associated with a tile in the supplied node's state."))

(defun net-tile-score (node)
  "Returns the sum of every tile's distance from the goal."
  (let ((node-state (state node)))
    (loop for tile in *tile-choices* summing (tile-goal-distance tile node-state))))

(defun net-node-score (node)
  "Returns the overall score for a particular node."
  (+ (cost (metadata node)) (net-tile-score node)))

(defmethod node-tile-score ((node puzzle-node) tile)
  (let ((num-moves (cost (metadata node))))
    (+ num-moves (tile-goal-distance tile (state node)))))

(defgeneric lowest-scored-action (node1 node2)
  (:documentation "Returns the node associated with the lowest cost by the node-tile-score method."))

(defmethod lowest-scored-action ((node1 puzzle-node) (node2 puzzle-node))
  (let ((node1-cost (net-node-score node1))
        (node2-cost (net-node-score node2)))
    (if (< node1-cost node2-cost) node1 node2)))

(defparameter *tile-choices* '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))

(defparameter *goal-state* '((1 2 3 4)
                             (5 6 7 8)
                             (9 10 11 12)
                             (13 14 15 :empty)))

(defparameter *minimal-goal-node*
  (make-instance 'puzzle-node
                 :state *goal-state*
                 :parent nil
                 :metadata (make-instance 'node-metadata :action :none)))

(defvar *action-choices* '(:up :down :left :right))

(defun node-print-state (node)
  "Prints the current state of a node as a 4x4 grid."
  (format t "~{~{~a~6,6t~}~%~}~% (cost: ~a)~%" (state node) (net-node-score node)))

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
      (let* ((action-result (action-transform-state state action)))
        (if (action-legal-p action-result)
            (push action legal-actions-list))))
    legal-actions-list))

;; TODO: Once the frontier/explored sets are implemented, add them as parameters to this
;; and check for membership
(defun node-legal-actions (node)
  "Checks which actions are legal to take on a given node's state."
  (let ((current-state (state node)))
    (legal-actions current-state)))

(defun node-state-equal-p (node1 node2)
  "Checks if two nodes have equivalent states."
  (equal (state node1) (state node2)))

(defun goal-state-p (node)
  "Checks if a given node's state is a goal state."
  (let ((current-state (state node)))
    (equal *goal-state* current-state)))

;; TODO: Clean up this function by refactoring
;; I definitely seem to use :test #'node-state-equal-p a ton
(defun node-take-best-action (node frontier explored)
  "Returns the resulting node after executing the best (least costly by heuristic) action on it."
  (let* ((legal-actions (node-legal-actions node))
         (node-results (mapcar #'(lambda (result)
                                   (node-take-action node result))
                               legal-actions))
         (unvisited-nodes (remove-if #'(lambda (node)
                                         (or (member node frontier :test #'node-state-equal-p)
                                             (member node explored :test #'node-state-equal-p)))
                                     node-results))
         (best-action (if (member *minimal-goal-node* unvisited-nodes :test #'node-state-equal-p)
                          (find *minimal-goal-node* unvisited-nodes :test #'node-state-equal-p)
                          (if (null unvisited-nodes)
                              (return-from node-take-best-action (values nil nil))
                              (reduce #'lowest-scored-action unvisited-nodes)))))
    (format t "Best action:~%")
    (node-print-state best-action)
    (values best-action (remove best-action unvisited-nodes :test #'node-state-equal-p))))

(defun traverse-actions (node)
  "Traverses the actions leading up to the node and creates a list out of them."
  (nreverse (loop for current-node = node then (parent current-node)
                  collect (previous-action (metadata current-node))
                  until (null (parent current-node)))))

(defun next-valid-node (best-node frontier)
  (or best-node (reduce #'lowest-scored-action frontier)))


(defun solve-from-node (node)
  "Solves the puzzle starting at a given node."
  (let ((frontier (list node))
        (explored '()))
    (loop for current-node = node then (next-valid-node best-node frontier)
          for (best-node other-nodes) = (multiple-value-list (node-take-best-action current-node
                                                                                    frontier
                                                                                    explored))
          for i = 1 then (1+ i)
          do (format t "Iteration ~a~%" i)
          do (progn
               (format t "Current state:~%")
               (node-print-state current-node))
          if (null best-node)
            do (format t "Current node in frontier: ~a" (not (null (member current-node
                                                                           frontier
                                                                           :test #'node-state-equal-p))))
          do (pushnew current-node explored :test #'node-state-equal-p)
          do (setf frontier (remove current-node frontier :test #'node-state-equal-p))
          until (goal-state-p current-node)
          do (setf frontier (append frontier other-nodes))
          while (not (null frontier))
          finally (return (traverse-actions current-node)))))

(defun initial-node-from-state (state)
  "Makes an initial node with the given state."
  (make-instance 'puzzle-node :state state
                              :parent nil
                              :metadata (make-instance 'node-metadata
                                                       :action :start)))

;; TODO: Check if all of the actions are legal and decide what to do if they aren't
(defun take-actions-list (state actions)
  "Takes the list of actions on the given state. Short-circuits to nil if any actions are illegal."
  (loop for action in actions
        for result = (action-transform-state (or current-state state) action)
        for current-state = (getf result :state)
        unless (action-legal-p result) do (return nil)
          finally (return current-state)))

;; TODO: In the way of solving the puzzle, take all legal actions and compare them via scores
