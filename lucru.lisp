(ql:quickload '(:hunchentoot :html-template))

(defpackage :cars-site
    (:use :cl :hunchentoot :html-template))

(in-package :cars-site)


(defparameter *COOKIE-NAME* "ebwuoeir") ; abstract, nu spune nimic
(defparameter *COOKIE-VALABILITY* (* 60 60)) ; 1h
(defparameter *PASS* "somepass") ; parola de admin :)
(defparameter *PASSWORD-POST-PARAMETER* "pwd")
(defparameter *SITE-ROOT* 
  (if (equalp "Linux" (software-type))
      "~/cars-site/"
      "~/Documents/GitHub/cars-site/"))
(defparameter *db* "cars-site-db")

(defun get-site-file (name)
  (merge-pathnames name *SITE-ROOT*))




; incarc fisierul cu baza de date
(load "db")

; incarc baza de date de pe disc
(load-from-disk)





; asta il face pe Hunchentoot sa arunce erorile in debugger
(setf hunchentoot:*catch-errors-p* nil)

; incarc fisierul cu functiile web
(load "web")

; pornesc server-ul web, il tin minte in variabila speciala *web*
(defparameter *web* (make-instance 'easy-acceptor :port 8080))
(start *web*)
