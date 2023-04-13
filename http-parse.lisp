(defun cat (&rest x) (eval (cons 'concatenate (cons ''string  x))))
(defun url-decode (s)
    (if (string= "" s) ""
        (if (eq (char s 0) #\%)
            (cat (string (code-char (read-from-string (cat "#x" (subseq s 1 3))))) (url-decode (subseq s 3)))
            (cat (subseq s 0 1) (url-decode (subseq s 1))))))

;; LUMP PARSER

; String from characters
(defun sfc (&rest chars)
    (let ((s ""))
         (dolist (c chars s)
             (setq s (concatenate 'string s (string c))))))

; Python style split function
(defun split-at (string splitter &optional maxsplit)
    (reverse
         (let ((bits nil) (splitcount 0) (spl (length splitter)) (spi nil))
             (loop
                  (setq spi (search splitter string))
                  (if (null spi) (return bits))
                  (push (subseq string 0 spi) bits)
                  (setq string (subseq string (+ spi spl)))
                  (incf splitcount)
                  (if (and maxsplit (>= splitcount maxsplit)) (return bits)))
              (push string bits))))

; Only works on ALL of the request from the client
(defun parse-http (http)
    (let* ((s-hb (split-at http (sfc #\Return #\Newline) 1))
           (start-line (first s-hb))
           (h-b (split-at (second s-hb) (sfc #\Return #\Newline #\Return #\Newline) 1))
           (lump-h (first h-b))
           (body (second h-b))
           (h-lines (split-at lump-h (sfc #\Return #\Newline)))
           (headers (mapcar #'(lambda (x)
                                      (let* ((b (split-at x ": " 1)))
                                            (cons (intern (first b)) (second b)))) h-lines))
           (split-start (split-at start-line " "))
           (method (first split-start))
           (target (second split-start)))
           ; http version is ignored
          (list method target headers body)))

;; INCREMENTAL PARSER TODO
