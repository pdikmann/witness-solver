(declaim (optimize (speed 3) (safety 0)))

(load "common.lisp")
(load "neato.lisp")
(load "graph.lisp")
(load "path.lisp")

;;(add-cell "doc" '(0 1 3 4))
;;(add-cell "cool" '(1 2 4 5))

(defparameter *foo* (grid-graph 6 5))
(render *foo* "foo")
(defparameter *solutions* (find-path *foo* 0 8))
(render-solutions *foo* (list (nth 2 *solutions*)))


;; @todo: set up constraint-structures and solving/testing mechanisms
;; blocks (separate colors)
;; stars (pair up) (don't/do pack with same color blocks)
;; hexagons (collect)
;; tetrominos (enclose)
;; symmetry (don't collide with mirror image solution)

;; (time (progn (defparameter *solutions* (find-path *foo* 0 25)) (length *solutions*)))
