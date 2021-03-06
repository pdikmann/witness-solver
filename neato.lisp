;; parse into neato format
;; also, render / convert to images

;; -----------------------------------------------------------------------------
;; render data into neato files
(defun render-edge (g e)
  "needs to know about graph because edges only store node indices, not names"
  (let ((n1n
         (or (edge-n1-name e)
             (node-name (nth (edge-n1 e)
                             (graph-nodes g)))))
        (n2n
         (or (edge-n2-name e)
             (node-name (nth (edge-n2 e)
                             (graph-nodes g))))))
    (concatenate 'string n1n " -- " n2n
                 "[label=\"" (edge-label e) "\""
                 (if (edge-style e)
                     (concatenate 'string ","
                                  "style="
                                  (edge-style e))
                     ",style=bold")
                 (if (edge-color e)
                     (concatenate 'string ",color=" (edge-color e))
                     ",color=gray80")
                 ",fontcolor=gray80"
                 "];")))

(defun render-node (n)
  (concatenate 'string (string (node-name n))
               "[label=\"" (node-label n) "\","
               (when (node-pos n)
                 (concatenate 'string "pos=\""
                              (numstring (first (node-pos n))) ","
                              (numstring (second (node-pos n)))
                              "\","))
               "width=0.1,height=0.1,fixedsize=shape,"
               "shape=" (node-shape n) "];"))

(defun render-cell (g c)
  (let* ((node
          (concatenate 'string (string (node-name c))
                       "[label=\"" (node-label c) "\","
                       "width=0.1,height=0.1,fixedsize=shape,"
                       "shape=" (node-shape c) "];"))
         (edge-structs
          (mapcar #'(lambda (n)
                      (make-edge :n1-name (node-name c)
                                 :n2 n
                                 :style "invis"))
                  (cell-nodes c)))
         (edge-renders
          (mapcar #'(lambda (e) (render-edge g e))
                  edge-structs)))
    (apply #'concatenate 'string node edge-renders)))

(defun render-graph (g)
  (flet ((render-edge_ (e) (render-edge g e))
         (render-cell_ (c) (render-cell g c)))
    (concatenate 'string
                 "graph G {"
                 (collect (mapcar #'render-node (graph-nodes g)))
                 (collect (mapcar #'render-edge_ (graph-edges g)))
                 (collect (mapcar #'render-cell_ (graph-cells g)))
                 "}")))

;; -----------------------------------------------------------------------------
;; render neato files to images
(defun render (g filename)
  (with-input-from-string (s (render-graph g))
    (sb-ext:run-program "/usr/bin/neato"
                        (list "-T" "png"
                              "-o" (concatenate 'string filename ".png"))
                        :input s
                        :wait t)))

;; -----------------------------------------------------------------------------
;; rendering solutions onto a graph
;; (color edges, render neato files & images, compose images)
(defun uncolor-edges (g)
  (mapcar #'(lambda (e) (setf (edge-color e) nil))
          (graph-edges g)))

(defun color-edge (e)
  (setf (edge-color e) "red"))

(defun color-solution (g s)
  (flet ((paint (cell)
           (if (null (cdr cell))
               nil
               (let ((e (assoc-edge (graph-edges g) (first cell) (second cell))))
                 (when e (color-edge e))))))
    (mapl #'paint s)))


(defun append-solutions (filenames)
  (sb-ext:run-program "/usr/bin/convert"
                      (append (list "-append") 
                              filenames
                              (list "solutions.png"))
                      :wait t))

(defun rm-temps (filenames)
  (sb-ext:run-program "/bin/rm" filenames :wait t))

(defun render-solutions (g sols)
  (let* ((fnames (mapcar #'(lambda (i) (format nil "temp~2,'0d" i))
                         (loop for i from 0 to (length sols) collect i)))
         (pngnames (mapcar #'(lambda (s) (concatenate 'string s ".png"))
                           fnames)))
    (mapcar #'(lambda (s fname)
                (uncolor-edges g)
                (color-solution g s)
                (render g fname))
            sols
            fnames)
    (append-solutions pngnames)
    (rm-temps pngnames)))
