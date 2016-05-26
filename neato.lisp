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
