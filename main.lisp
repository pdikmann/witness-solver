(load "common.lisp")
(load "neato.lisp")
(load "graph.lisp")
(load "path.lisp")

;; -----------------------------------------------------------------------------
;; construction
(defun grid-graph (x y)
  (let ((g (make-graph)))
    (labels ((grid (x y &optional (cx 1) (cy 1))
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
                    (unless (eq cy y)  ; connect all rows except last
                      (add-edge g index (+ x index)))
                    (grid x y 1 (1+ cy)))
                   ;; add to current row
                   (t
                    (add-node g (default-node (numstring index)))
                    (pin-last-node g (list cx cy))
                    (add-edge g index (1+ index))
                    (unless (eq cy y)  ; connect all rows except last
                      (add-edge g index (+ x index)))
                    (grid x y (1+ cx) cy))))))
      (grid x y))
    (setf (edge-color (first (graph-edges g))) "red") ; DEBUG
    g))

;;(add-cell "doc" '(0 1 3 4))
;;(add-cell "cool" '(1 2 4 5))

(defun render (g filename)
  (with-input-from-string (s (render-graph g))
    (sb-ext:run-program "/usr/bin/neato"
                        (list "-T" "png"
                              "-o" (concatenate 'string filename ".png"))
                        :input s)))

(defparameter *foo* (grid-graph 2 2))
(render *foo* "foo")
(defparameter *solutions* (find-path *foo* 0 3))

