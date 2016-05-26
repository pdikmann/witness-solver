;; -----------------------------------------------------------------------------
;; wayfinding
(defparameter *path* '())

(defun adjacent-edges (g node-index)
  (remove-if-not #'(lambda (e) (or (eq (edge-n1 e) node-index)
                                   (eq (edge-n2 e) node-index)))
                 (graph-edges g)))

(defun adjacent-nodes (g node-index)
  (let ((edges (adjacent-edges g node-index)))
    (mapcar #'(lambda (e)
                (if (eq (edge-n1 e) node-index)
                    (edge-n2 e)
                    (edge-n1 e)))
            edges)))

(defun find-path_ (g start-node end-node &optional (path nil) (this-node start-node))
  "helper for find-path. uses *path* to accumulate valid paths"
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
  (let ((*path* nil))
    (find-path_ g start-node end-node)
    *path*))

(defun find-simple (g start-node end-node &optional (prev nil) (path nil) (this-node start-node))
  ;; @todo finish this
  "don't setf - return something nicer"
  (let* ((next-nodes (adjacent-nodes g this-node))
         (available-nodes (remove-if #'(lambda (e) (member e path))
                                     next-nodes)))
    (cond
      ((eq this-node end-node)          ; win!
       this-node)
      ((null available-nodes)           ; fail!
       nil)
      (t                                ; continue working
       (cons this-node
             (mapcar #'(lambda (nn)
                         (find-simple g
                                      start-node
                                      end-node
                                      this-node
                                      (cons this-node path)
                                      nn))
                     available-nodes))))))
