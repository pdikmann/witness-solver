;; -----------------------------------------------------------------------------
;; making nodes and edges
(defun add-node (node)
  "returns index of node in *nodes*"
  (setf *nodes* (reverse (cons node (reverse *nodes*))))
  (1- (length *nodes*)))

(defun add-edge (n1 n2 &optional (style nil))
  (setf *edges* (cons (make-edge :n1 n1
                                 :n2 n2
                                 :style style)
                      *edges*))
  *edges*)

(defun add-cell (label connections)
  (let ((ci (add-node (plain-node label))))
    (mapcar #'(lambda (ni) (add-edge ci ni "invis"))
            connections))
  ;; @todo store in proper struct, add to *cells* list.
  )

(defun pin-node (label position)
  "expects position as '(x y)"
  (let ((n (first (member label *nodes* :key #'node-label :test #'equal))))
    (setf (node-pos n) position)))

(defun pin-last-node (position)
  "expects position as '(x y)"
  (let ((n (first (last *nodes*))))
    (setf (node-pos n) position)))

(defun clear-graph ()
  (setf *nodes* '())
  (setf *edges* '())
  (setf *cells* '()))
