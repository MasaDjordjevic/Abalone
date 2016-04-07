; Allow cl-who and parenscript to work together
(setf *js-string-delimiter* #\")

(defparameter *tabla* (kreirajTablu 5))

(defun-ajax echo (data) (*ajax-processor* :callback-data :response-text)
  (let (
        ;(a (format t "rezultat: ~S~%" (eval data)))
        ;(b (format t "data: ~S~%"  data))
        ;(c (eval data))
        (d (string-to-list data))
        )
    (format nil "~S~%" (stampaj
                        (setq *tabla*
                              (if (null (odigraj-potez (car d) (cadr d) *tabla*))
                                  *tabla*
                                  (odigraj-potez (car d) (cadr d) *tabla*)))))))

(defun-ajax reset (data) (*ajax-processor* :callback-data :response-text)
  (format nil "~S~%" (stampaj (setq *tabla* (kreirajTablu 5)))))

;;; NIJE NAS KOD ;;;

(defparameter *ajax-processor*
  (make-instance 'ajax-processor :server-uri "/repl-api"))

(defun string-to-list (string)
  "Returns a list of the data items represented in the given list."
  (let ((the-list nil) ;; we'll build the list of data items here
        (end-marker (gensym))) ;; a unique value to designate "done"
    (loop (multiple-value-bind (returned-value end-position)
                               (read-from-string string nil end-marker)
            (when (eq returned-value end-marker)
              (return the-list))
            ;; if not done, add the read thing to the list
            (setq the-list
                  (append the-list (list returned-value)))
            ;; and chop the read characters off of the string
            (setq string (subseq string end-position))))))

(defparameter *server*
  (start (make-instance 'easy-acceptor :address "localhost" :port 8080)))

(setq *dispatch-table* (list 'dispatch-easy-handlers
                             (create-ajax-dispatcher *ajax-processor*)))
