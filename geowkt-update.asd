;;; -*- Mode: Lisp -*-
(in-package :asdf-user)

(asdf:defsystem :geowkt-update
  :author "Manuel Giraud <manuel@ledu-giraud.fr>"
  :description "Library to parse Geo Well-known-text and update the
geowkt library"
  :serial t
  :depends-on (:parse-number :drakma)
  :components ((:file "packages")
               (:file "geowkt-update")))
