(in-package :cars-site)


(defparameter *COOKIE-NAME* "ebwuoeir") ; abstract, no further info provided
(defparameter *COOKIE-VALABILITY* (* 60 60)) ; 1h
(defparameter *PASS* "somepass") ; admin password :)
(load "~/cslv") ; supersedes admin password, local to machine
(defparameter *PASSWORD-POST-PARAMETER* "pwd")
(defparameter *SITE-ROOT* 
  (if (equalp "Linux" (software-type))
      "~/cars-site/"
      "~/Documents/GitHub/cars-site/"))
(defparameter *db* "the-db")
(defparameter *NO-LOGIN* t)

(defun get-site-file (name)
  (merge-pathnames name *SITE-ROOT*))






; tell Hunchentoot to throw errors/conditions in debugger
(setf hunchentoot:*catch-errors-p* nil)

; start acceptor, keep a reference in the special variable *web*
(defparameter *web* (make-instance 'easy-acceptor :port 8080))
(start *web*)
