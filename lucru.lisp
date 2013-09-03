(ql:quickload '(:hunchentoot))

(defpackage :cars-site
    (:use :cl :hunchentoot))

(in-package :cars-site)

(defclass attribute ()
  ((name :initarg :name :initform (error "Care este numele atributului?") :accessor name)
   (id :initarg :id :initform (get-next-attribute-id) :reader id)
   (att-type :initarg :type :initform 'string :accessor att-type)))

(defparameter *all-attributes* nil)

(defvar *attribute-ids* 0)

(defun get-next-attribute-id ()
  (incf *attribute-ids*))

(defun add-attribute (att)
  (if (find-attribute-by-name (name att))
      (error "Atributul cu acest nume este deja introdus"))
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
  ((name :initarg :name :initform (error "Care este numele vehiculului?") :accessor name)
   (id :initarg :id :initform (get-next-vehicle-id) :reader id)
   (attributes :initform '())))

(defparameter *all-vehicles* nil)

(defvar *vehicle-ids* 0)

(defun get-next-vehicle-id ()
  (incf *vehicle-ids*))

(defun add-vehicle (vehicle)
  (push vehicle *all-vehicles*)
  vehicle)

(defun find-vehicle (i)
  (find i *all-vehicles* :test #'(lambda (to-find y) (= (id y) to-find))))

(add-vehicle (make-instance 'vehicle :name "Volkswagen Golf 4 1.9 TDI 90CP"))
(add-vehicle (make-instance 'vehicle :name "Mazda 3 1.6 105CP"))
(add-vehicle (make-instance 'vehicle :name "Dacia Duster Laureate 1.5 dCi 110CP"))

(defun set-attribute-on-vehicle (vehicle att-name att-value)
  (if (find-attribute-by-name att-name)
      (setf 
       (slot-value vehicle 'attributes)
       (acons att-name att-value (slot-value vehicle 'attributes)))
      (error "Nu exista acest atribut!")))

(defun get-attribute-on-vehicle (vehicle att-name)
  (let ((att (assoc att-name (slot-value vehicle 'attributes) :test #'string-equal)))
    (if att
      (cdr att)
      nil)))

(defun query-vehicles (criteria) 
  (let ((result (copy-seq *all-vehicles*)))
    (loop
       for crit-pair in criteria
       do 
	 (setf result 
	       (remove-if-not 
		#'(lambda (vehicle) 
		    (equalp 
		     (get-attribute-on-vehicle vehicle (car crit-pair))
		     (cdr crit-pair)))
		result)))
    result))

(set-attribute-on-vehicle (nth 0 *all-vehicles*) "Forma" "SUV")
(set-attribute-on-vehicle (nth 0 *all-vehicles*) "Tip combustibil" "Motorina")
(set-attribute-on-vehicle (nth 0 *all-vehicles*) "Capacitate cilindrica" "1495")
(set-attribute-on-vehicle (nth 0 *all-vehicles*) "Numar usi" "5")
(set-attribute-on-vehicle (nth 0 *all-vehicles*) "Culoare" "Beige special")

(set-attribute-on-vehicle (nth 1 *all-vehicles*) "Forma" "Hatchback")
(set-attribute-on-vehicle (nth 1 *all-vehicles*) "Tip combustibil" "Benzina")
(set-attribute-on-vehicle (nth 1 *all-vehicles*) "Capacitate cilindrica" "1595")
(set-attribute-on-vehicle (nth 1 *all-vehicles*) "Numar usi" "3")
(set-attribute-on-vehicle (nth 1 *all-vehicles*) "Culoare" "Blue beton")

(set-attribute-on-vehicle (nth 2 *all-vehicles*) "Forma" "Break")
(set-attribute-on-vehicle (nth 2 *all-vehicles*) "Tip combustibil" "Motorina")
(set-attribute-on-vehicle (nth 2 *all-vehicles*) "Capacitate cilindrica" "1896")
(set-attribute-on-vehicle (nth 2 *all-vehicles*) "Numar usi" "5")
(set-attribute-on-vehicle (nth 2 *all-vehicles*) "Culoare" "Bleumarin plictisitor")
