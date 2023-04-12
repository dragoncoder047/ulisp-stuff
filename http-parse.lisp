;; LUMP PARSER

(defun sfc (&rest chars)
    (let ((s ""))
         (dolist (c chars s)
             (setq s (concatenate 'string s (string c))))))

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

(defun parse-http (http)
    (let* ((s-hb (split-at http (sfc #|#\Return|# #\Newline) 1))
           (start-line (first s-hb))
           (h-b (split-at (second s-hb) (sfc #|#\Return|# #\Newline #|#\Return|# #\Newline) 1))
           (lump-h (first h-b))
           (body (second h-b))
           (h-lines (split-at lump-h (sfc #|#\Return|# #\Newline)))
           (headers (mapcar #'(lambda (x)
                                      (let* ((b (split-at x ": " 1)))
                                            (cons (intern (first b)) (second b)))) h-lines))
           (split-start (split-at start-line " "))
           (method (first split-start))
           (target (second split-start)))
           ;; http version is ignored
          (list method target headers body)))

;; INCREMENTAL PARSER TODO
