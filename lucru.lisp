(ql:quickload '(:hunchentoot))

(defpackage :cars-site
    (:use :cl :hunchentoot))

(in-package :cars-site)

(defclass attribute ()
  ((name :initarg :name :initform (error "What is the attribute's name?") :accessor name)
   (id :initarg :id :initform (get-next-attribute-id) :reader id)
   (att-type :initarg :type :initform 'string :accessor att-type)))

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

(add-attribute (make-instance 'attribute :name "Capacitate cilindrica"))
(add-attribute (make-instance 'attribute :name "Numar usi"))
(add-attribute (make-instance 'attribute :name "Forma"))
(add-attribute (make-instance 'attribute :name "Tip combustibil"))
(add-attribute (make-instance 'attribute :name "Culoare"))



(defclass vehicle ()
  ((name :initarg :name :initform (error "What is the vehicle's name?") :accessor name)
   (id :initarg :id :initform (get-next-vehicle-id) :reader id) ; does it need initarg?
   (attributes :initform '())))

(defparameter *all-vehicles* nil)

(defvar *vehicle-ids* 0)

(defun get-next-vehicle-id ()
  (incf *vehicle-ids*))

(defun add-vehicle (vehicle)
  (push vehicle *all-vehicles*)
  vehicle)

(defun find-vehicle (i)
  "Finds a vehicle by its id"
  (find i *all-vehicles* :test #'(lambda (to-find y) (= (id y) to-find))))

(defun query-vehicles (criteria) 
  "Query all vehicles that meet certain criteria; the criteria should be a list of pairs (attribute_id, attribute_value), but I'm not checking this ATM"
  nil)

(add-vehicle (make-instance 'vehicle :name "Volkswagen Golf 4 1.9 TDI 90CP"))
(add-vehicle (make-instance 'vehicle :name "Mazda 3 1.6 105CP"))
(add-vehicle (make-instance 'vehicle :name "Dacia Duster Laureate 1.5 dCi 110CP"))

