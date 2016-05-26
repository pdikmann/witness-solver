;; -----------------------------------------------------------------------------
;; models
(defparameter *nodes* '())
(defparameter *edges* '())
(defparameter *cells* '())

(defstruct node
  (name "NONAME")
  (label "NOLABEL")
  (shape "circle")
  (style nil)
  (pos nil)
  (special nil))

(defstruct edge
  (n1 0)
  (n2 0)
  (style nil))

(defun default-node (label)
  (make-node :name (string (gensym))
             :label label
             :shape "circle"))

(defun plain-node (label)
  (make-node :name (string (gensym))
             :label label
             :shape "plaintext"))

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
            connections)))

(defun pin-node (label position)
  "expects position as '(x y)"
  (let ((n (first (member label *nodes* :key #'node-label :test #'equal))))
    (setf (node-pos n) position)))

(defun pin-last-node (position)
  "expects position as '(x y)"
  (let ((n (first (last *nodes*))))
    (setf (node-pos n) position)))

;; -----------------------------------------------------------------------------
;; parse into neato format
;; helper
(defun envelop (front back content)
  (concatenate 'string front content back))

(defun collect (lst)
  (apply #'concatenate 'string lst))

(defun numstring (num)
  (format nil "~d" num))

;; ----------------------------------------
;; graph
(defun graph (content)
  (envelop "graph G {" "}" content))

(defun edge (e)
  (let ((n1n (node-name (nth (edge-n1 e) *nodes*)))
        (n2n (node-name (nth (edge-n2 e) *nodes*))))
    (if (edge-style e)
        (concatenate 'string n1n " -- " n2n "[style=" (edge-style e) "];")
        (concatenate 'string n1n " -- " n2n ";"))))

(defun node (n)
  (concatenate 'string (string (node-name n))
               "[label=" (node-label n) ","
               (when (node-pos n)
                 (concatenate 'string "pos=\""
                              (numstring (first (node-pos n))) ","
                              (numstring (second (node-pos n)))
                              "\","))
               "shape=" (node-shape n) "];"))

(defun render-nodes ()
  (graph (concatenate 'string
                      (collect (mapcar #'node *nodes*))
                      (collect (mapcar #'edge *edges*)))))

;; -----------------------------------------------------------------------------
;; construction
(labels ((grid (x y &optional (cx 1) (cy 1))
           (let ((index (+ (1- cx)
                           (* x (1- cy)))))
             (cond ((and (eq cx x)
                         (eq cy y))
                    ;; finish grid
                    (add-node (default-node (numstring index)))
                    (pin-last-node (list cx cy)))
                   ((eq cx x)
                    ;; finish row
                    (add-node (default-node (numstring index)))
                    (pin-last-node (list cx cy))
                    (unless (eq cy y) ; unless last row: connect rows
                      (add-edge index (+ x index)))
                    (grid x y 1 (1+ cy)))
                   (t
                    ;; add to current row
                    (add-node (default-node (numstring index)))
                    (pin-last-node (list cx cy))
                    (add-edge index (1+ index))
                    (unless (eq cy y) ; unless last row: connect rows
                      (add-edge index (+ x index)))
                    (grid x y (1+ cx) cy))))))
  (grid 3 3))

(add-cell "doc" '(0 1 3 4))
(add-cell "cool" '(1 2 4 5))

(with-input-from-string (s (render-nodes))
  (sb-ext:run-program "/usr/bin/neato" (list "-T" "png" "-o" "stream.png") :input s)
  s)
