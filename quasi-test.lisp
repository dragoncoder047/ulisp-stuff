(defun test (x)
    (format t "~a => ~s~%" x (eval (read-from-string x))))

(defvar num 1)
(defvar lis '(1 2 3))
(test "`(foo num)") ;; (foo num)
(test "`(foo ,num)") ;; (foo 1)
(test "`(,num foo)") ;; (1 foo)
(test "`(foo ,lis)") ;; (foo (1 2 3))
(test "`(,lis foo)") ;; ((1 2 3) foo)
(test "`(foo ,@lis)") ;; (foo 1 2 3)
(test "`(,@lis foo)") ;; (1 2 3 foo)
(test "`(foo ,lis num)") ;; (foo (1 2 3) num)
(test "`(foo ,@lis num)") ;; (foo 1 2 3 num)
(test "`(,@lis foo num)") ;; (1 2 3 foo num)
(test "`(,@lis foo ,num)") ;; (1 2 3 foo 1)
(test "`(foo ,lis ,num)") ;; (foo (1 2 3) 1)
(test "`(foo ,@lis ,num)") ;; (foo 1 2 3 1)
(test "`(foo ,lis ,@num)") ;; (foo (1 2 3) . 1)

;; The quine test: should evaluate to itself:
(let ((let '`(let ((let ',let)) ,let))) `(let ((let ',let)) ,let))
;; source: https://3e8.org/pub/scheme/doc/Quasiquotation%20in%20Lisp%20(Bawden).pdf
