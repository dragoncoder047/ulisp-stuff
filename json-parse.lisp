(defun whitespace-char-p (ch)
    (if (search (list ch) '(#\Space #\Newline #\Return #\Tab)) t nil))

(defun skip-whitespace (str)
    (let (ch)
         (loop
             (setq ch (peek-char t str))
             (if (whitespace-char-p ch)
                 (read-char stream)
                 (return)))))

(defun expect-char (str ch &optional (xp ch))
    (if (eq (peek-char t str) ch) (read-char str) (error "unexpected ~c (expected ~a)" (peek-char t str) xp)))

(defun json-parse-list (str &optional rest)
    (expect-char str #\[)
    (reverse
        (loop
            (skip-whitespace str)
            (let* ((itm (json-parse str)))
                (skip-whitespace str)
                (case (peek-char t str)
                    (#\]
                        (read-char str)
                        (return (cons itm rest)))
                    (#\,
                        (read-char str)
                        (push itm rest))
                    (t
                        (error "unexpected ~c after list item" (peek-char t str))))))))

(defun json-parse-object (str &optional rest) 
    (expect-char str #\{)
    (reverse
        (loop
            (skip-whitespace str)
            (let* (key obj)
                (setq key (json-parse-string str))
                (skip-whitespace str)
                (expect-char str #\:)
                (skip-whitespace str)
                (setq obj (cons key (json-parse str)))
                (skip-whitespace str)
                (case (peek-char t str)
                    (#\}
                        (read-char str)
                        (return (cons obj rest)))
                    (#\,
                        (read-char str)
                        (push obj rest))
                    (t
                        (error "unexpected ~c after object entry" (peek-char t str))))))))

(defun json-get-hex-escape (str)
    (let ((xs '(nil nil nil nil))
          (get-hex (lambda (_) (read-char str)))
          (validate (lambda (digit)
                        (case digit
                            ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                                (- (char-code digit) (char-code #\0)))
                            ((#\a #\b #\c #\d #\e #\f)
                                (- (char-code digit) (char-code #\a)))
                            ((#\A #\B #\C #\D #\E #\F)
                                (- (char-code digit) (char-code #\A)))
                            (t
                                (error "unexpected non-hex digit ~c" digit))))))
        (setq xs (mapcar get-hex xs))
        (setq xs (mapcar validate xs))
        (code-char (+ (nth 3 xs) (* 16 (+ (third xs) (* 16 (+ (second xs) (* 16 (first xs))))))))))

(defun json-parse-escape-char (str)
    (expect-char str #\\)
    (let ((ch (read-char str)))
        (case ch
            (#\" #\")
            (#\\ #\\)
            (#\/ #\/)
            (#\b #\Backspace)
            (#\f #\FF)
            (#\n #\Newline)
            (#\r #\Return)
            (#\t #\Tab)
            (#\u
                (json-get-hex-escape str))
            (t
                (error "unexpected escape control character ~c" ch)))))

(defun json-parse-string (str)
    (expect-char str #\")
    (let ((ch nil) (cstr ""))
        (loop
            (case (peek-char t str)
                (#\\
                    (setq ch (json-parse-escape-char str)))
                (#\"
                    (read-char str)
                    (return cstr))
                (t
                    (setq ch (read-char str))))
            (setq cstr (concatenate 'string cstr (string ch))))))

(defun json-parse-number (str)
    (if (null (search (list (peek-char t str)) '(#\- #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))) (error "unexpected ~c" (read-char str)))
    (let ((digits '(#\0))
          (decimal '(#\0))
          (exponent '(#\0))
          (negp nil)
          (expnegp nil)
          (phase 'initial))
        (loop
            (case (peek-char t str)
                ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                    (case phase
                        ((initial digits)
                            (setq phase 'digits)
                            (push (read-char str) digits))
                        (decimal
                            (push (read-char str) decimal))
                        ((exponent exp-initial)
                            (setq phase 'exponent)
                            (push (read-char str) exponent))
                        (t
                            (error "malformed number"))))
                (#\-
                    (case phase
                        (initial
                            (setq negp t)
                            (read-char str))
                        (exp-initial
                            (setq expnegp t)
                            (read-char str))
                        (t
                            (error "malformed number"))))
                (#\+
                    (case phase
                        ((initial exp-initial)
                            (read-char str))
                        (t
                            (error "malformed number"))))
                (#\.
                    (case phase
                        (digits
                            (setq phase 'decimal)
                            (read-char str))
                        (t
                            (error "malformed number"))))
                ((#\e #\E)
                    (case phase
                        ((digits decimal)
                            (setq phase 'exp-initial)
                            (read-char str))
                        (t
                            (error "malformed number"))))
                (t
                    (return))))
        (setq digits (reverse digits))
        (setq decimal (reverse decimal))
        (setq exponent (reverse exponent))
        (read-from-string
            (format nil "~c~{~d~}.~{~d~}e~c~{~d~}~%"
                (if negp #\- #\+)
                digits
                decimal
                (if expnegp #\- #\+)
                exponent))))

(defun json-parse-boolean (str)
    (case (peek-char t str)
        (#\t
            (expect-char str #\t)
            (expect-char str #\r)
            (expect-char str #\u)
            (expect-char str #\e)
            t)
        (#\f
            (expect-char str #\f)
            (expect-char str #\a)
            (expect-char str #\l)
            (expect-char str #\s)
            (expect-char str #\e)
            nil)
        (#\n
            (expect-char str #\n)
            (expect-char str #\u)
            (expect-char str #\l)
            (expect-char str #\l)
            nil)
        (t
            (error "unexpected ~c" (read-char str)))))

(defun json-parse (str)
    (skip-whitespace str)
    (case (peek-char t str)
        (#\[
            (json-parse-list str))
        (#\{
            (json-parse-object str))
        (#\"
            (json-parse-string str))
        ((#\t #\f #\n)
            (json-parse-boolean str))
        ((#\- #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
            (json-parse-number str))
        (t
            (error "unexpected ~c" (read-char str)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Tests

#|

(defvar mystr "
              {
                  \"he\\u9212llo\": \"world\",
                  \"number\": 123,
                  \"foo\": 123.456E+34,
                  \"mylist\"    : [1,2,3], \"help\"
        :
        true
            }")
(format t "~d~%~%~s~%" mystr (json-parse (make-string-input-stream mystr)))

;|#
