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

(defun add-cell (g index nodes)
  ;; @todo store in proper struct, add to *cells* list.
  ;; use to visualize in-cell symbols
  ;; don't put edges into same graph structure - use some meta / second-layer graph for that
  ;; only render them, don't have them in the structure used for solving
  (let ((c (make-cell :name (concatenate 'string "cell" (numstring index))
                      :label (concatenate 'string "c_" (numstring index))
                      :nodes nodes)))
    (setf (graph-cells g)
                (cons c (graph-cells g)))))

(defun pin-node (g label position)
  "expects position as '(x y)"
  (let ((n (first (member label (graph-nodes g) :key #'node-label :test #'equal))))
    (setf (node-pos n) position)))

(defun pin-last-node (g position)
  "expects position as '(x y)"
  (let ((n (first (last (graph-nodes g)))))
    (setf (node-pos n) position)))

;; -----------------------------------------------------------------------------
;; construction
(defun grid-graph (x y)
  "construct a grid-shaped graph of dimensions `x` (width) and `y` (height)"
  (let ((g (make-graph)))
    (labels ((grid (x y &optional (cx 1) (cy 1)) ; no need to pass x y (in closure)
               (let ((index (+ (1- cx)
                               (* x (1- cy)))))
                 (cond
                   ;; finish grid
                   ((and (eq cx x)
                         (eq cy y))
                    (add-node g (default-node (numstring index)))
                    (pin-last-node g (list cx cy))
                    g)
                   ;; finish row
                   ((eq cx x)
                    (add-node g (default-node (numstring index)))
                    (pin-last-node g (list cx cy))
                    (unless (eq cy y)   ; connect all rows except last
                      (add-edge g index (+ x index)))
                    (grid x y 1 (1+ cy)))
                   ;; add to current row
                   (t
                    (add-node g (default-node (numstring index)))
                    (pin-last-node g (list cx cy))
                    (add-edge g index (1+ index))
                    (unless (eq cy y)   ; connect all rows except last
                      (add-edge g index (+ x index)))
                    (grid x y (1+ cx) cy)))))
             (cell-nodes (i x)   ; don't need to pass x y (in closure)
               (let* ((base
                       (+ i (floor (/ i (1- x)))))
                      (nodes ; ccw starting at lowest node index
                       (list base (+ base 1)
                             (+ base x 1) (+ base x))))
                 nodes))
             (cells (x y)        ; don't need to pass x y (in closure)
               (loop for i from 0 below (* (1- x) (1- y)) do
                    (add-cell g i (cell-nodes i x)))))
      (grid x y)
      (cells x y))
    g))
