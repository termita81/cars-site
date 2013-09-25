(ql:quickload '(:hunchentoot :html-template))

(defpackage :cars-site
    (:use :cl :hunchentoot :html-template))

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

; salveaza baza de date pe disc, sau o incarca
(defun serialize ()
  "versiune penibila, dar functionala"
  (with-output-to-string (s)
    (format s "; atribute")
    (loop for a in (get-all-attributes)
	 do (format s "~%(add-attribute (make-instance 'attribute :name \"~a\" :id ~a :att-type '~a))"
		    (name a)
		    (id a)AL
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


; pornesc server-ul web, il tin minte in variabila speciala *web*
(defparameter *web* (make-instance 'easy-acceptor :port 8080))
(start *web*)

; incarc baza de date de pe disc
(load-from-disk)



; asta il face pe Hunchentoot sa arunce erorile in debugger
(setf hunchentoot:*catch-errors-p* nil)

; pentru debug: salveaza ultimul obiect REQUEST
(defparameter *LAST-REQUEST* nil)

(defun is-logged-in ()
  (cookie-in *COOKIE-NAME*))



(pushnew (create-prefix-dispatcher "/login" 'web-login) *dispatch-table*)
(defun web-login ()
  (setf *LAST-REQUEST* *REQUEST*)
  (if (is-logged-in)
      (redirect "/")
      (let ((pass (post-parameter *PASSWORD-POST-PARAMETER*))
	    (wrong nil))
	(if pass
	    (if (string= pass *PASS*)
		(progn
		  (set-cookie *COOKIE-NAME* :value t :expires 
			      (+ *COOKIE-VALABILITY* (get-universal-time)))
		  (redirect "/"))
		(setf wrong t)))
	(with-output-to-string (s)
	  (html-template:fill-and-print-template 
	   (get-site-file "login.tmpl") 
	   (list :wrong-password wrong) 
	   :stream s)
	  s))))

(pushnew (create-prefix-dispatcher "/admin" 'web-admin) *dispatch-table*)
(defun web-admin ()
  (setf *LAST-REQUEST* *REQUEST*)
  (when (not (is-logged-in))
      (redirect "/login"))
  (let ((cmd (or (get-parameter "cmd")
		  (post-parameter "cmd")))
	(edit-vehicle))
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
	  do (set-attribute-on-vehicle 
	      (parse-integer (get-parameter "id")) 
	      (name att) 
	      (get-parameter (name att)))))
      ((equal cmd "save-data")
       (persist-to-disk))
      (t nil))
    (with-output-to-string (s)
      (html-template:fill-and-print-template
       (get-site-file "admin.tmpl")
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
					   :att-value ,(get-attribute-on-vehicle (id edit-vehicle) (name x))))
			   (get-all-attributes))))
	 )
       :stream s)
       s)))


; mecanism... oarecum indoielnic
; care sa trateze "/"
; de ce indoielnic? parca nu-mi suna mie bine
; dar ce vreau:
; - anumite comenzi sa fie tratate speciale
; - resursele statice sa fie servite
; - / sa duca intr-o anumita pagina
; - sa se genereze 404 pentru celelalte chestii, care nu intra aici
; mecanismul asta permite indeplinirea acestor cerinte
(pushnew 'root-dispatcher *dispatch-table*)

(defun root-dispatcher (request)
  (when (equalp (script-name request) "/")
    #'root-handler))

(defun root-handler ()
  (with-output-to-string (s)
    (html-template:fill-and-print-template
     (get-site-file "index.tmpl")
     `()
     :stream s)
    s))
