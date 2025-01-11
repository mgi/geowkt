;;; -*- Mode: Lisp -*-
(in-package :asdf-user)

(asdf:defsystem :geowkt
  :author "Manuel Giraud <manuel@ledu-giraud.fr>"
  :description "Geo Well-known-text database"
  :serial t
  :components ((:file "packages")
               (:file "geowkt")
               (:file "db")
               (:file "name-db")))
