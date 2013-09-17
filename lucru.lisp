(ql:quickload '(:hunchentoot :html-template))

(defpackage :cars-site
    (:use :cl :hunchentoot :html-template))

(in-package :cars-site)

; atribute

(let ((attribute-id-seq 0))
  (defclass attribute ()
    ((name :initarg :name :initform (error "Care este numele atributului?") :accessor name)
     (id :initarg :id :initform (incf attribute-id-seq) :reader id)
     (att-type :initarg :type :initform 'string :accessor att-type))))

(let (all-attributes)
  (defun find-attribute-by-name (name)
    (find name all-attributes 
	  :test #'(lambda (to-find item) (string= (name item) to-find))))
  (defun add-attribute (att)
    (if (find-attribute-by-name (name att))
	(error "Atributul cu acest nume este deja introdus")
	(push att all-attributes))
    att)
  (defun search-attribute-by-name (partial-name)
    (loop 
       for a in all-attributes
       when 
	 (search partial-name (name a) :test #'string-equal) 
       collect a))
  (defun get-all-attributes ()
    "Numai pentru debug"
    all-attributes))


; vehicule

(let ((vehicle-id-seq 0))
  (defclass vehicle ()
    ((name :initarg :name :initform (error "Care este numele vehiculului?") :accessor name)
     (id :initarg :id :initform (incf vehicle-id-seq) :reader id)
     (attributes :initform '()))))

(let (all-vehicles)  
  (defun find-vehicle-by-id (i)
    (find i all-vehicles :test #'(lambda (to-find y) (= (id y) to-find))))
  (defun find-vehicle-by-name (name)
    (find name all-vehicles 
	  :test #'(lambda (to-find item) (string= (name item) to-find))))
  (defun add-vehicle (vehicle)
    (if (find-vehicle-by-name (name vehicle))
	(error "Vehiculul cu acest nume este deja introdus!")
	(push vehicle all-vehicles))
    vehicle)
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
    (let ((result (copy-seq all-vehicles)))
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
  (defun get-all-vehicles ()
    "Numai pentru debug"
    all-vehicles))


; date de test
(progn
  (add-attribute (make-instance 'attribute :name "Capacitate cilindrica"))
  (add-attribute (make-instance 'attribute :name "Numar usi"))
  (add-attribute (make-instance 'attribute :name "Forma"))
  (add-attribute (make-instance 'attribute :name "Tip combustibil"))
  (add-attribute (make-instance 'attribute :name "Culoare"))
  
  (add-vehicle (make-instance 'vehicle :name "Volkswagen Golf 4 1.9 TDI 90CP"))
  (add-vehicle (make-instance 'vehicle :name "Mazda 3 1.6 105CP"))
  (add-vehicle (make-instance 'vehicle :name "Dacia Duster Laureate 1.5 dCi 110CP"))

  (setf v (nth 0 (get-all-vehicles)))
  (set-attribute-on-vehicle v "Forma" "SUV")
  (set-attribute-on-vehicle v "Tip combustibil" "Motorina")
  (set-attribute-on-vehicle v "Capacitate cilindrica" "1495")
  (set-attribute-on-vehicle v "Numar usi" "5")
  (set-attribute-on-vehicle v "Culoare" "Bej special")
  
  (setf v (nth 1 (get-all-vehicles)))
  (set-attribute-on-vehicle v "Forma" "Hatchback")
  (set-attribute-on-vehicle v "Tip combustibil" "Benzina")
  (set-attribute-on-vehicle v "Capacitate cilindrica" "1595")
  (set-attribute-on-vehicle v "Numar usi" "3")
  (set-attribute-on-vehicle v "Culoare" "Blue beton")
  
  (setf v (nth 2 (get-all-vehicles)))
  (set-attribute-on-vehicle v "Forma" "Break")
  (set-attribute-on-vehicle v "Tip combustibil" "Motorina")
  (set-attribute-on-vehicle v "Capacitate cilindrica" "1896")
  (set-attribute-on-vehicle v "Numar usi" "5")
  (set-attribute-on-vehicle v "Culoare" "Bleumarin plictisitor"))


; print-vehicle
(defun print-vehicle (vehicle)
  (format t "Vehicul cu id ~a: ~a~%~(~a~%~)" 
	  (id vehicle) 
	  (name vehicle) 
	  (loop 
	     for att in (slot-value vehicle 'attributes)
	     collect (concatenate 'string (car att) ": " (cdr att) '(#\Newline)))))

; pornesc server-ul web
(defparameter *web* (make-instance 'easy-acceptor :port 8080))
(start *web*)



; login
(defconstant +COOKIE-NAME+ "ebwuoeir") ; abstract, nu spune nimic
(defconstant +PASS+ "somepass") ; parola de admin :)
(defconstant +PASSWORD-POST-PARAMETER+ "pwd")

(defun is-logged-in ()
  (cookie-in +COOKIE-NAME+))

(defun web-login ()
  (if (is-logged-in)
      (redirect "/")
      (let ((pass (post-parameter +PASSWORD-POST-PARAMETER+))
	    (wrong nil))
	(if pass
	    (if (string= pass +PASS+)
		(progn
		  (set-cookie +COOKIE-NAME+ :value "t")
		  (redirect "/"))
		(setf wrong t)))
	(with-output-to-string (s)
	  (html-template:fill-and-print-template #p"c:/Users/rpopa/Documents/GitHub/cars-site/login.tmpl" '(:wrong-password wrong) :stream s)
	  s))))

(pushnew (create-prefix-dispatcher "/login" 'web-login) *dispatch-table*)


;  (format nil "~:[Nu ~;~]e logat" (is-logged-in)))
