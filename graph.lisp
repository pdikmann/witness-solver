;; -----------------------------------------------------------------------------
;; making nodes and edges
(defun add-node (g node)
  "returns index of node in *nodes*"
  (setf (graph-nodes g) (reverse (cons node
                                       (reverse (graph-nodes g)))))
  (1- (length (graph-nodes g))))

(defun add-edge (g n1 n2 &optional (style nil))
  (setf (graph-edges g)
        (cons (make-edge :n1 n1
                         :n2 n2
                         :label (concatenate 'string
                                             (format nil "~2,'0d" n1)
                                             (format nil "~2,'0d" n2))
                         :style style)
              (graph-edges g))))

(defun add-cell (g label connections)
  (let ((ci (add-node g (plain-node label))))
    (mapcar #'(lambda (ni) (add-edge g ci ni "invis"))
            connections))
  ;; @todo store in proper struct, add to *cells* list.
  )

(defun pin-node (g label position)
  "expects position as '(x y)"
  (let ((n (first (member label (graph-nodes g) :key #'node-label :test #'equal))))
    (setf (node-pos n) position)))

(defun pin-last-node (g position)
  "expects position as '(x y)"
  (let ((n (first (last (graph-nodes g)))))
    (setf (node-pos n) position)))

;; (defun clear-graph (g)
;;   (setf *nodes* '())
;;   (setf *edges* '())
;;   (setf *cells* '()))
