(in-package  :cars-site)


(defun get-template-file (name)
  (merge-pathnames name (merge-pathnames "tmpl/" *SITE-ROOT*)))


; pentru debug: salveaza ultimul obiect REQUEST
(defparameter *LAST-REQUEST* nil)

(defun is-logged-in ()
  (or *NO-LOGIN* (cookie-in *COOKIE-NAME*)))

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
	   (get-template-file "login.tmpl") 
	   (list :wrong-password wrong) 
	   :stream s)
	  s))))

(pushnew (create-prefix-dispatcher "/login" 'web-login) *dispatch-table*)

(defun att-type-from-int (the-int)
  (cond ((= the-int 2) 'number)
	((= the-int 3) 'bool)
	(t 'string)))

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
       (let ((name (get-parameter "attribute"))
	     (att-type (att-type-from-int (parse-integer (get-parameter "att-type")))))
	 (add-attribute (make-instance 'attribute :name name :att-type att-type))))
      ((equal cmd "edit-vehicle")
       (setf edit-vehicle (get-vehicle-by-id (parse-integer (get-parameter "id")))))
      ((equal cmd "set-attributes")
       (setf edit-vehicle (get-vehicle-by-id (parse-integer (get-parameter "id"))))       
       (loop for att in (get-all-attributes)
	  for id-att = (id att)
	  for value = 
	    (or (get-parameter (write-to-string id-att))
		"off")
	  when value
	  do (set-attribute-on-vehicle 
	      (parse-integer (get-parameter "id")) 
	      id-att
	      value)))
      ((equal cmd "save-data")
       (persist-to-disk))
      (t nil))
    (with-output-to-string (s)
      (html-template:fill-and-print-template
       (get-template-file "admin.tmpl")
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
				(let ((val (get-attribute-on-vehicle (id edit-vehicle) (id x))))
				  `(:att-id ,(id x)
				    :att-name ,(name x)
				    :att-bool ,(eq 'bool (att-type x))
				    :att-checked ,(and (eq 'bool (att-type x)) (string-equal val "on"))
				    :att-value ,val)))
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
     (get-template-file "index.tmpl")
     `()
     :stream s)
    s))

(defun root-dispatcher (request)
  (when (equalp (script-name request) "/")
    #'root-handler))

(pushnew 'root-dispatcher *dispatch-table*)
