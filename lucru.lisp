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

(defun search-attribute-by-name (partial-name)
  (loop 
     for a in *all-attributes* 
     when 
       (search partial-name (name a) :test #'string-equal) 
     collect a))

(defclass attribute ()
  ((name :initarg :name :initform (error "What is the attribute's name?") :accessor name)
   (id :initarg :id :initform (get-next-attribute-id) :reader id)
   (type :initarg :type :initform 'string :accessor type)))

(add-attribute (make-instance 'attribute :name "Capacitate cilindrica"))
(add-attribute (make-instance 'attribute :name "Numar usi"))
(add-attribute (make-instance 'attribute :name "Forma"))
(add-attribute (make-instance 'attribute :name "Tip combustibil"))
(add-attribute (make-instance 'attribute :name "Culoare"))



(defparameter *all-cars* nil)

(defvar *car-ids* 0)

(defun get-next-car-id ()
  (incf *car-ids*))

(defclass car ()
  ((name :initarg :name :initform (error "What is the car's name?") :accessor name)
   (id :initarg :id :initform (get-next-car-id) :reader id) ; does it need initarg?
   (attributes :initform '())))

(defun add-car (car)
  (push car *all-cars*)
  car)

(defun find-car (i)
  "Finds a car by its id"
  (find i *all-cars* :test #'(lambda (to-find y) (= (id y) to-find))))

(defun query-cars (criteria) 
  "Query all cars that meet certain criteria; the criteria should be a list of pairs (attribute_id, attribute_value), but I'm not checking this ATM"
  nil)

(add-car (make-instance 'car :name "Volkswagen Golf 4 1.9 TDI 90CP"))
(add-car (make-instance 'car :name "Mazda 3 1.6 105CP"))
(add-car (make-instance 'car :name "Dacia Duster Laureate 1.5 dCi 110CP"))

