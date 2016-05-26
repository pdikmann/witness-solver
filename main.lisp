(defparameter *nodes* '())
(defparameter *edges* '())
(defparameter *cells* '())

(defstruct edge
  n1 n2 style)

(defun add-node (label)
  (setf *nodes* (reverse (cons label 
                               (reverse *nodes*))))
  *nodes*)

(defun add-edge (n1 n2)
  (setf *edges* (cons (list n1 n2) *edges*))
  *edges*)

;; -----------------------------------------------------------------------------
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

(defun edge (lst)
  (let ((n1n (string (nth (car lst) *nodes*)))
        (n2n (string (nth (cadr lst) *nodes*))))
    (concatenate 'string n1n " -- " n2n ";")))

(defun node (n)
  (concatenate 'string (string n) "[shape=circle];"))

;; (defun cell (lst)
;;   (let ((c (string (car lst))))
;;     (node c)
;;     ))

;; "{ edge[ style=invis ]; ... }"

(defun render-nodes ()
  (graph (concatenate 'string
                      (collect (mapcar #'node *nodes*))
                      (collect (mapcar #'edge *edges*)))))

;; -----------------------------------------------------------------------------
;; (add-node 'jim)
;; (add-node 'bimbo)
;; (add-node 'wim)
;; (add-edge 0 1)
;; (add-edge 0 2)

(labels ((grid (x y &optional (cx 1) (cy 1))
           (let ((index (+ (1- cx)
                           (* x (1- cy)))))
             (cond ((and (eq cx x)
                         (eq cy y))
                    ;; finish grid
                    (add-node (numstring index)))
                   ((eq cx x)
                    ;; finish row
                    (add-node (numstring index))
                    (unless (eq cy y) ; unless last row: connect rows
                      (add-edge index (+ x index)))
                    (grid x y 1 (1+ cy)))
                   (t
                    ;; add to current row
                    (add-node (numstring index))
                    (add-edge index (1+ index))
                    (unless (eq cy y) ; unless last row: connect rows
                      (add-edge index (+ x index)))
                    (grid x y (1+ cx) cy))))))
  (grid 5 5))


(with-input-from-string (s (render-nodes))
  (sb-ext:run-program "/usr/bin/neato" (list "-T" "png" "-o" "stream.png") :input s))
