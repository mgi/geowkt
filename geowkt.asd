;;; -*- Mode: Lisp -*-
(in-package :asdf-user)

(asdf:defsystem :geowkt
  :name "geo-wkt"
  :author "Manuel Giraud <manuel@ledu-giraud.fr>"
  :description "Library to parse Geo Well-known-text"
  :serial t
  :depends-on (:parse-number :drakma)
  :components ((:file "package")
	       (:file "geowkt")))
