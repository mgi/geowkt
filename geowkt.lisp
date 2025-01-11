(in-package :geowkt)

(defparameter *db* (make-hash-table))
(defparameter *name-db* (make-hash-table :test 'equal))

(defun wkt (code-or-string)
  (if (integerp code-or-string)
      (gethash code-or-string *db*)
      (gethash code-or-string *name-db*)))
