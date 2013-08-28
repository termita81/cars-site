(ql:quickload '(:hunchentoot))

(defpackage :cars
    (:use :cl :hunchentoot))

(in-package :cars)


(defparameter *all-attributes* nil)

(defvar *attribute-ids* 0)

(defun get-next-attribute-id ()
  (incf *attribute-ids*))

(defun add-attribute (att)
  "Should check for existing attribute"
  (if (find-attribute-by-name (name att))
      (error "There is already an attribute by this name"))
  (push att *all-attributes*)
  att)

(defun find-attribute-by-name (name)
  (find name *all-attributes* :test #'(lambda (to-find item) (string= (name item) to-find))))

(add-attribute (make-instance 'attribute :name "Capacitate cilindrica"))
(add-attribute (make-instance 'attribute :name "Numar usi"))
(add-attribute (make-instance 'attribute :name "Forma"))
(add-attribute (make-instance 'attribute :name "Tip combustibil"))

