;; parse into neato format
;; also, render / convert to images

;; -----------------------------------------------------------------------------
;; render data into neato files
(defun render-edge (g e)
  (let ((n1n (node-name (nth (edge-n1 e) (graph-nodes g))))
        (n2n (node-name (nth (edge-n2 e) (graph-nodes g)))))
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
               "color=transparent,"
               "width=0.1,height=0.1,fixedsize=shape,"
               "shape=" (node-shape n) "];"))

(defun render-graph (g)
  (flet ((edge (e) (render-edge g e)))
    (concatenate 'string
                 "graph G {"
                 (collect (mapcar #'render-node (graph-nodes g)))
                 (collect (mapcar #'edge (graph-edges g)))
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
