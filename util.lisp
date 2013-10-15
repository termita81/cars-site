(in-package  :cars-site)

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (i args) (princ i s))))

(defun mksym (&rest args)
  (values (intern (apply #'mkstr args))))


#|
(def-route nume (args) body)
==>
(progn 
  (defun nume (args)
    body)
  (pushnew (create-prefix-dispatcher (mkstr "/" 'nume) 'nume) *dispatch-table*))

(defmacro def-route (prefix (&rest args) &body body)
  `(progn
     (defun ,prefix ,args
	 (setf *LAST-REQUEST* *REQUEST*)
	 ,@body)
     (pushnew (create-prefix-dispatcher (mkstr "/" ',prefix) ',prefix) *dispatch-table*)))
vreau sa pun alt nume la functie, derivat din prefix => vreau sa-l calculez inainte de progn

(defmacro def-route (prefix (&rest args) &body body)
  (let ((bad-fname (format nil "WEB-~@:(~a~)" prefix)))
    `(progn
     (defun ,bad-fname ,args
	 (setf *LAST-REQUEST* *REQUEST*)
	 ,@body)
     (pushnew (create-prefix-dispatcher (mkstr "/" ',prefix) ',bad-fname) *dispatch-table*))))

problema este ca "fur" binding-ul bad-fname, care poate sa fi fost definit inainte, asa ca trebuie sa lucrez cu gensym

(defmacro def-route (prefix (&rest args) &body body)
  (let ((gen-symbol (gensym)))
    `(let ((,gen-symbol (format nil "WEB-~@:(~a~)" prefix)))
       (progn
	 (defun ,gen-symbol ,args
	   (setf *LAST-REQUEST* *REQUEST*)
	   ,@body)
	 (pushnew (create-prefix-dispatcher (mkstr "/" ',prefix) ',gen-symbol) *dispatch-table*)))))
|#
