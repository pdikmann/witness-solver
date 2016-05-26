(load "common.lisp")
(load "neato.lisp")
(load "graph.lisp")
(load "path.lisp")

;;(add-cell "doc" '(0 1 3 4))
;;(add-cell "cool" '(1 2 4 5))

(defparameter *foo* (grid-graph 3 3))
(render *foo* "foo")
(defparameter *solutions* (find-path *foo* 0 8))
(render-solutions *foo* *solutions*)
