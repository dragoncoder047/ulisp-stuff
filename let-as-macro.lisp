(defmacro my-let (bindings &body forms)
    `(funcall #'(lambda ,(mapcar #'(lambda (x)
                                           (if (consp x)
                                               (car x)
                                               x))
                                 bindings)
                        ,@forms)
              ,@(mapcar #'(lambda (x)
                                  (if (and (consp x) (consp (cdr x)))
                                      (cadr x)
                                      nil))
                        bindings)))

(defmacro my-let* (bindings &body forms)
    (if bindings
        `(my-let (,(first bindings)) (my-let* ,(rest bindings) ,@forms))
        `(progn ,@forms)))

#|
(print (macroexpand '(my-let ((x 1) (y 2) (z 3)) (print x) (print y) (print z))))
(terpri)
(print (macroexpand '(my-let* ((x 1) (y 2) (z (* x y 17))) (print x) (print y) (print z))))
(terpri)
;|#
