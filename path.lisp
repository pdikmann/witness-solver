;; -----------------------------------------------------------------------------
;; wayfinding
(defparameter *path* '())

(defun adjacent-edges (g node-index)
  (declare (type (unsigned-byte 8) node-index))
  (the list
       (remove-if-not #'(lambda (e) (or (eq (edge-n1 e) node-index)
                                        (eq (edge-n2 e) node-index)))
                      (graph-edges g))))

(defun adjacent-nodes (g node-index)
  (declare (type (unsigned-byte 8) node-index))
  (let ((edges (adjacent-edges g node-index)))
    (mapcar #'(lambda (e)
                (if (eq (edge-n1 e) node-index)
                    (edge-n2 e)
                    (edge-n1 e)))
            edges)))

(defun find-path_ (g start-node end-node &optional (path nil) (this-node start-node))
  "helper for find-path. uses *path* to accumulate valid paths"
  (declare (type (unsigned-byte 8) this-node start-node end-node)
           (type list path))
  (let* ((next-nodes (adjacent-nodes g this-node))
         (available-nodes (remove-if #'(lambda (e) (member e path))
                                     next-nodes)))
    (cond
      ((eq this-node end-node)          ; win!
       (setf *path* (cons (cons this-node path)
                          *path*))
       t)
      ((null available-nodes)           ; fail!
       nil)
      (t                                ; continue
       (mapcar #'(lambda (nn)
                   (find-path_ g
                               start-node
                               end-node
                               (cons this-node path)
                               nn))
               available-nodes)))))

(defun find-path (g start-node end-node)
  "return all valid paths (solutions) on the graph from start-node to end-node"
  (let ((*path* nil))
    (find-path_ g start-node end-node)
    *path*))

(defun cell-neighbors (g cell)
  "CELL: cell grid index"
  (let ((w (1- (graph-width g)))
        (h (1- (graph-height g))))
    (remove-if #'null
               (list (unless (= (mod cell w) 0) ; cell is at left edge
                       (1- cell))
                     (unless (= (mod cell w) (1- w)) ; cell is at right edge
                       (1+ cell)) 
                     (unless (= (floor (/ cell w)) 0) ; cell at bottom
                       (- cell w)) 
                     (unless (= (floor (/ cell w)) (1- h)) ; cell at top
                       (+ cell w))
                     ))))

(defun separated? (c1 c2 solution)
  "C1, C2: cell structs of adjacent cells
SOLUTION: list of succesive nodes traced by solution path"
  (let* ( ;;(c1 (first (graph-cells *foo*)))
         ;;(c2 (second (graph-cells *foo*)))
         (border (intersection (cell-nodes c1)
                               (cell-nodes c2)))
         (n1 (first border))
         (n2 (second border)))
    (or (search (list n1 n2) solution)
        (search (list n2 n1) solution))))

(defun explore-block (g solution in
                      &optional
                        (out '())
                        (index (1- (length in))))
  "IN: list of cell grid indices in block, e.g. '(0) on very first call"
  (let* ((current-cell (nth index in))
         (neighbors (cell-neighbors g current-cell)) ; indices
         (out? #'(lambda (other-cell)
                   (separated? (nth current-cell (graph-cells g))
                               (nth other-cell (graph-cells g))
                               solution )))
         (present-in? #'(lambda (new-index)
                          (member new-index in)))
         (present-out? #'(lambda (new-index)
                           (member new-index out)))
         (new-in (remove-if present-in?
                            (remove-if out? neighbors)))
         (new-out (remove-if present-out?
                             (remove-if (complement out?) neighbors))))
    (if (and (null new-in)
             (= index 0))
        (list in (append new-out out))  ; done
        (explore-block g
                       solution
                       (append new-in in)
                       (append new-out out)
                       (1- (+ index (length new-in)))))))

(defun cell-blocks_ (g solution done todo done-flat)
  (let* ((result (explore-block g
                                solution
                                (list (first todo))))
         (found (first result))
         (new-flat (append found done-flat))
         (left (remove-if #'(lambda (e)
                              (member e new-flat))
                          (append (rest todo)
                                  (second result)))))
    (if (null left)
        (cons found done)
        (cell-blocks_ g solution
                      (cons found done)
                      left
                      new-flat))))

(defun cell-blocks (g solution)
  "return a list of blocks (connected cells) that are separated by the trail of the solution"
  (cell-blocks_ g solution nil '(0) nil))

;; (defun find-simple (g start-node end-node &optional (prev nil) (path nil) (this-node start-node))
;;   ;; @todo finish this
;;   "don't setf - return something nicer"
;;   (let* ((next-nodes (adjacent-nodes g this-node))
;;          (available-nodes (remove-if #'(lambda (e) (member e path))
;;                                      next-nodes)))
;;     (cond
;;       ((eq this-node end-node)          ; win!
;;        this-node)
;;       ((null available-nodes)           ; fail!
;;        nil)
;;       (t                                ; continue working
;;        (cons this-node
;;              (mapcar #'(lambda (nn)
;;                          (find-simple g
;;                                       start-node
;;                                       end-node
;;                                       this-node
;;                                       (cons this-node path)
;;                                       nn))
;;                      available-nodes))))))
