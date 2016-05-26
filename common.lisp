;; -----------------------------------------------------------------------------
;; models
(defparameter *nodes* '())
(defparameter *edges* '())
(defparameter *cells* '())

(defstruct node
  (name "NONAME")
  (label "NOLABEL")
  (shape "circle")
  (style nil)
  (pos nil)
  (special nil))

(defstruct edge
  (n1 0)
  (n2 0)
  (style nil))

(defun default-node (label)
  (make-node :name (string (gensym))
             :label label
             :shape "circle"))

(defun plain-node (label)
  (make-node :name (string (gensym))
             :label label
             :shape "plaintext"))
