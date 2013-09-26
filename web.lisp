(in-package  :cars-site)



; pentru debug: salveaza ultimul obiect REQUEST
(defparameter *LAST-REQUEST* nil)

(defun is-logged-in ()
  (cookie-in *COOKIE-NAME*))

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

(pushnew (create-prefix-dispatcher "/login" 'web-login) *dispatch-table*)

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

(pushnew (create-prefix-dispatcher "/admin" 'web-admin) *dispatch-table*)

; mecanism... oarecum indoielnic
; care sa trateze "/"
; de ce indoielnic? parca nu-mi suna mie bine
; dar ce vreau:
; - anumite comenzi sa fie tratate speciale
; - resursele statice sa fie servite
; - / sa duca intr-o anumita pagina
; - sa se genereze 404 pentru celelalte chestii, care nu intra aici
; mecanismul asta permite indeplinirea acestor cerinte
(defun root-handler ()
  (with-output-to-string (s)
    (html-template:fill-and-print-template
     (get-site-file "index.tmpl")
     `()
     :stream s)
    s))

(defun root-dispatcher (request)
  (when (equalp (script-name request) "/")
    #'root-handler))

(pushnew 'root-dispatcher *dispatch-table*)
