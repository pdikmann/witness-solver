;; -----------------------------------------------------------------------------
;; models
(defstruct graph
  (nodes nil)
  (edges nil)
  (cells nil))

(defstruct node
  (name "NONAME")
  (label "NOLABEL")
  (shape "plaintext")
  (style nil)
  (pos nil)
  ;; (special nil)
  )

(defstruct edge
  (label "x")
  (n1 0)
  (n2 0)
  ;; @todo maybe add node names here for easier rendering?
  (color nil)
  (style nil)
  ;; (special nil)
  )

(defstruct (cell (:include node))
  (nodes nil)
  ;; (special nil)
  )

(defparameter *graph* (make-graph))
;; (defparameter *nodes* '())
;; (defparameter *edges* '())
;; (defparameter *cells* '())

(defun default-node (label)
  (make-node :name (string (gensym))
             :label label
             :shape "plaintext"))

(defun plain-node (label)
  (make-node :name (string (gensym))
             :label label
             :shape "plaintext"))

;; -----------------------------------------------------------------------------
;; helper
;; (defun envelop (front back content)
;;   (concatenate 'string front content back))

(defun collect (lst)
  (apply #'concatenate 'string lst))

(defun numstring (num)
  (format nil "~d" num))

(defun connectsp (edge n1 n2)
  "does edge `edge` connect nodes `n1` and `n2`?"
  (or (and (eq (edge-n1 edge) n1)
           (eq (edge-n2 edge) n2))
      (and (eq (edge-n1 edge) n2)
           (eq (edge-n2 edge) n1))))

(defun assoc-edge (edges n1 n2)
  (cond
    ((null edges) nil)
    ((connectsp (first edges)
                n1 n2)
     (first edges))
    (t (assoc-edge (rest edges) n1 n2))))
