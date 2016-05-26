;; -----------------------------------------------------------------------------
;; parse into neato format

;; (defun graph (content)
;;   (envelop "graph G {" "}" content))

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
