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
  (defun get-vehicle-by-id (id)
    (loop for v in all-vehicles when (= id (id v)) return v))
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

; pornesc server-ul web, il tin minte in variabila speciala *web*
(defparameter *web* (make-instance 'easy-acceptor :port 8080))
(start *web*)



; login
(defparameter *COOKIE-NAME* "ebwuoeir") ; abstract, nu spune nimic
(defparameter *COOKIE-VALABILITY* (* 60 60)) ; 1h
(defparameter *PASS* "somepass") ; parola de admin :)
(defparameter *PASSWORD-POST-PARAMETER* "pwd")
(defparameter *TEMPLATE-ROOT* "~/cars-site/")

(defun get-template (name)
  (merge-pathnames name *TEMPLATE-ROOT*))

; asta il face pe Hunchentoot sa arunce erorile in debugger
(setf hunchentoot:*catch-errors-p* nil)

; pentru debug: salveaza ultimul obiect REQUEST
(defparameter *LAST-REQUEST* nil)

(pushnew (create-prefix-dispatcher "/login" 'web-login) *dispatch-table*)
(defun web-login ()
  (setf *LAST-REQUEST* *REQUEST*)
  (if (cookie-in *COOKIE-NAME*)
      (redirect "/")
      (let ((pass (post-parameter *PASSWORD-POST-PARAMETER*))
	    (wrong nil))
	(if pass
	    (if (string= pass *PASS*)
		(progn
		  (set-cookie *COOKIE-NAME* :value t :expires (+ *COOKIE-VALABILITY* (get-universal-time)))
		  (redirect "/"))
		(setf wrong t)))
	(with-output-to-string (s)
	  (html-template:fill-and-print-template 
	   (get-template "login.tmpl") 
	   (list :wrong-password wrong) 
	   :stream s)
	  s))))

(pushnew (create-prefix-dispatcher "/admin" 'web-admin) *dispatch-table*)
(defun web-admin ()
  (setf *LAST-REQUEST* *REQUEST*)
  (let ((cmd (or (get-parameter "cmd")
		  (post-parameter "cmd")))
	(edit-vehicle)
	(params))
    (cond
      ((equal cmd "add-vehicle")
       (let ((name (get-parameter "vehicle")))
	 (add-vehicle (make-instance 'vehicle :name name))))
      ((equal cmd "add-attribute")
       (let ((name (get-parameter "attribute")))
	 (add-attribute (make-instance 'attribute :name name))))
      ((equal cmd "edit-vehicle")
       (setf edit-vehicle (get-vehicle-by-id (parse-integer (get-parameter "id")))))
      ((equal cmd "set-attributes")
       (setf edit-vehicle (get-vehicle-by-id (parse-integer (get-parameter "id"))))       
       (loop for att in (get-all-attributes)
	  do (set-attribute-on-vehicle edit-vehicle (name att) (get-parameter (name att)))))
      (t nil))
    (with-output-to-string (s)
      (html-template:fill-and-print-template
       (get-template "admin.tmpl")
       `(:vehicles 
	 ,(mapcar #'(lambda (x) 
		      `(:id ,(id x) :name ,(name x))) 
		  (get-all-vehicles))
	 :attributes 
	 ,(mapcar #'(lambda (x) 
		      `(:id ,(id x) :name ,(name x) :att-type ,(att-type x))) 
		  (get-all-attributes))
	 ,@(when edit-vehicle		 
		 `(:edit-vehicle ,(name edit-vehicle)
		   :edit-vehicle-id ,(id edit-vehicle)
		   :edit-vehicle-attrs
		   ,(mapcar #'(lambda (x)
			       `(:att-name ,(name x)
					   :att-value ,(get-attribute-on-vehicle edit-vehicle (name x))))
			   (get-all-attributes))))
	 )
       :stream s)
       s)))
