(in-package :geowkt)

(defparameter *db* (make-hash-table))

(defun wkt-from-code (code)
  (gethash code *db*))
