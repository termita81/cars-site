(in-package :cars-site)

; atribute
(let ((all-attributes)
      (attribute-id-seq 0))
  (defclass attribute ()
    ((name :initarg :name :initform (error "Care este numele atributului?") :accessor name)
     (id :initarg :id :initform (incf attribute-id-seq) :reader id)
     (att-type :initarg :att-type :initform 'string :accessor att-type)))
  (defun find-attribute-by-name (name)
    (find name all-attributes 
	  :test #'(lambda (to-find item) (string= (name item) to-find))))
  (defun add-attribute (att)
    (if (find-attribute-by-name (name att))
	(error "Atributul cu acest nume este deja introdus")
	(push att all-attributes))
    (when (<= attribute-id-seq (id att))
      (setf attribute-id-seq (1+ (id att))))
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
(let ((all-vehicles)
      (vehicle-id-seq 0))
  (defclass vehicle ()
    ((name :initarg :name :initform (error "Care este numele vehiculului?") :accessor name)
     (id :initarg :id :initform (incf vehicle-id-seq) :reader id)
     (attributes :initform '())))
  (defun find-vehicle-by-id (i)
    (find i all-vehicles :test #'(lambda (to-find y) (= (id y) to-find))))
  (defun find-vehicle-by-name (name)
    (find name all-vehicles 
	  :test #'(lambda (to-find item) (string= (name item) to-find))))
  (defun add-vehicle (vehicle)
    (if (find-vehicle-by-name (name vehicle))
	(error "Vehiculul cu acest nume este deja introdus!")
	(push vehicle all-vehicles))
    (when (<= vehicle-id-seq (id vehicle))
      (setf vehicle-id-seq (1+ (id vehicle))))
    vehicle)
  (defun set-attribute-on-vehicle (vehicle-id att-name att-value)
    (if (find-attribute-by-name att-name)
	(let ((vehicle (find-vehicle-by-id vehicle-id)))
	  (when vehicle
	    (setf 
	     (slot-value vehicle 'attributes)
	     (acons att-name att-value (slot-value vehicle 'attributes)))))
	(error "Nu exista acest atribut!")))
  (defun get-attribute-on-vehicle (vehicle-id att-name)
    (let* 
	((vehicle (get-vehicle-by-id vehicle-id))
	 (att (assoc att-name (slot-value vehicle 'attributes) :test #'string-equal)))
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
		       (get-attribute-on-vehicle (id vehicle) (car crit-pair))
		       (cdr crit-pair)))
		  result)))
      result))
  (defun get-vehicle-by-id (id)
    (loop for v in all-vehicles when (= id (id v)) return v))
  (defun get-all-vehicles ()
    "Numai pentru debug"
    all-vehicles))


; salveaza baza de date pe disc, sau o incarca
(defun serialize ()
  "versiune penibila, dar functionala"
  (with-output-to-string (s)
    (format s "; atribute")
    (loop for a in (get-all-attributes)
	 do (format s "~%(add-attribute (make-instance 'attribute :name \"~a\" :id ~a :att-type '~a))"
		    (name a)
		    (id a)
		    (att-type a)))
    (format s "~%~%; vehicule")
    (loop for v in (get-all-vehicles)
	 do (format s "~%(add-vehicle (make-instance 'vehicle :name \"~a\" :id ~a))"
		    (name v)
		    (id v)))
    (format s "~%~%; atributele vehiculelor")
    (loop for v in (get-all-vehicles)
	 do (loop for a in (get-all-attributes)
	       do (let* ((id (id v))
			 (name (name a))
			 (val (get-attribute-on-vehicle id name)))
		    (when (and val (not (equal "NIL" val)))
		      (format s "~%(set-attribute-on-vehicle ~a \"~a\" \"~a\")" id name val)))))
    s))

(defun persist-to-disk ()
  (with-open-file (g (get-site-file *db*) :direction :output 
		     :if-exists :supersede :if-does-not-exist :create)
    (format g "~a" (serialize))))

(defun load-from-disk ()
  (load (get-site-file *db*)))

; print-vehicle
(defun print-vehicle (vehicle)
  (format t "Vehicul cu id ~a: ~a~%~(~a~%~)" 
	  (id vehicle) 
	  (name vehicle) 
	  (loop 
	     for att in (slot-value vehicle 'attributes)
	     collect (concatenate 'string (car att) ": " (cdr att) '(#\Newline)))))
