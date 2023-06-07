(defun compile-glob (enm)
    (let ((out (list "")))
         (loop
             (if (zerop (length enm)) (return))
             (if (eq (char enm 0) #\*)
                 (progn
                     (push "" out)
                     (setq enm (subseq enm 1))))
             (if (zerop (length enm)) (return))
             (setf (first out) (concatenate 'string (first out) (subseq enm 0 1)))
             (setq enm (subseq enm 1)))
         (reverse out)))

(defun glob-p (name glob)
    (loop
        (unless name (return nil))
        (unless glob (return t))
        (let ((i (search (first glob) name)))
             (unless i (return nil))
             (setq name (subseq name (+ i (length (first glob)))))
             (setq glob (rest glob)))))
                 
#|
(defvar g "left*move*")
(print (compile-glob g))
(print (glob-p "left_motor_move_complete" (compile-glob g))) ;; T
(print (glob-p "left_motor_set_speed_failed" (compile-glob g))) ;; NIL
(print (glob-p "right_motor_move_complete" (compile-glob g))) ;; NIL
(print (glob-p "leftmove" (compile-glob g))) ;; T
;|#
