;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-

;;; Copyright (c) 2013, Razvan Popa.

;;; Disclaimer: any way you use this, you're on your own. Howgh.

(in-package :cl-user)

(defpackage :cars-site-asd
  (:use :cl :asdf))

(in-package :cars-site-asd)

(ql:quickload '(:hunchentoot :html-template :cl-json))

(defpackage :cars-site
  (:use :cl :hunchentoot :html-template :cl-json))

(defsystem :cars-site
  :serial t
  :version "0.0.1"
  :description "Some new cars choosing site"
  :depends-on (:hunchentoot
	       :html-template)
  :components ((:file "start")
	       (:file "util")
	       (:file "web")
	       (:file "db")))


