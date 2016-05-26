(load "common.lisp")
(load "neato.lisp")
(load "graph.lisp")

;; -----------------------------------------------------------------------------
;; construction
(clear-graph)

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
  (grid 4 4))

(add-cell "doc" '(0 1 3 4))
(add-cell "cool" '(1 2 4 5))

(with-input-from-string (s (render-nodes))
  (sb-ext:run-program "/usr/bin/neato" (list "-T" "png" "-o" "stream.png") :input s))
