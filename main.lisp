(load "common.lisp")
(load "neato.lisp")
(load "graph.lisp")
(load "path.lisp")

;;(add-cell "doc" '(0 1 3 4))
;;(add-cell "cool" '(1 2 4 5))

(defparameter *foo* (grid-graph 3 3))
(render *foo* "foo")
(defparameter *solutions* (find-path *foo* 0 8))
;; (render-solutions *foo* *solutions*)


;; @todo: set up constraint-structures and solving/testing mechanisms
;; blocks (separate colors)
;; stars (pair up) (don't/do pack with same color blocks)
;; hexagons (collect)
;; tetrominos (enclose)
;; symmetry (don't collide with mirror image solution)

