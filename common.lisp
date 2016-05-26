;; -----------------------------------------------------------------------------
;; models
(defstruct graph
  (nodes nil)
  (edges nil))

(defstruct node
  (name "NONAME")
  (label "NOLABEL")
  (shape "circle")
  (style nil)
  (pos nil)
  (special nil))

(defstruct edge
  (label "x")
  (n1 0)
  (n2 0)
  (color nil)
  (style nil))

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
