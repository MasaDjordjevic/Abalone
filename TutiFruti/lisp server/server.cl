
(defpackage :jank-repl
  (:use :cl :hunchentoot :cl-who :parenscript :smackjack))


(in-package :jank-repl)

; Allow cl-who and parenscript to work together
(setf *js-string-delimiter* #\")

(defparameter *ajax-processor*
  (make-instance 'ajax-processor :server-uri "/repl-api"))

(defmacro simple-json-bind ((&rest vars) stream &body body)
  (let ((cur-dec (gensym))
        (key-handler
         `(lambda (json-string)
            (let ((lisp-string
                   (funcall json:*json-identifier-name-to-lisp*
                            json-string)))
              ;; On recognizing a variable name, the key handler sets
              ;; the value handler to be a function which,
              ;; in its turn, assigns the decoded value to the
              ;; corresponding variable.  If no variable name matches
              ;; the key, the value handler is set to skip the value.
              (json:set-custom-vars
                  :object-value
                    (cond
                      ,@(loop for var in vars
                          collect
                           `((string= lisp-string ,(symbol-name var))
                             (lambda (value)
                               (setq ,var value))))
                      (t (constantly t))))))))
    `(let ((,cur-dec (json:current-decoder))
           ,@vars)
       (json:bind-custom-vars
           (:internal-decoder ,cur-dec
            :beginning-of-object (constantly t)
            :object-key ,key-handler
            :end-of-object (constantly t))
         (json:decode-json ,stream))
       ,@body)))

(defparameter *data* "")

(defparameter *tip* "")
(defparameter *polja* "")

(defparameter *send-data* '((tip . 0) (polja . 0)))


(defun-ajax reset (data) (*ajax-processor* :callback-data :response-text)
  (format nil "~S~%" (stampaj (setq *tabla* (kreiraj-tablu 5)))))


(defun-ajax hello-world (data) (*ajax-processor* :callback-data :response-text)
  (progn
    (setq *data* data)
    (format nil "~S~%" "Hello world!")))

(defun-ajax test (data) (*ajax-processor* :callback-data :response-text)
  (progn
    (json:with-decoder-simple-list-semantics
                  (with-input-from-string     
                      (s data)
                    (simple-json-bind (tip polja) s
                                      (progn
                                        (setq *tip* tip)
                                        (setq *polja* polja)))))
    (setq *data* data)
    (format nil "~S~%" (json:encode-json-to-string
                        '#( ((foo . (1 2 3)) (bar . t) (baz . #\!))
                           "quux" 4/17 4.25)))))

(defun-ajax prvi (data)(*ajax-processor* :callback-data :response-text)
  (progn
    (json:with-decoder-simple-list-semantics
                  (with-input-from-string     
                      (s data)
                    (simple-json-bind (tip polja) s
                                      (progn
                                        (setq *tip* tip)
                                        (setq *polja* polja)))))
    (setq *data* data)
    (setq *tip* "prvi")
    (rplacd (assoc 'tip *send-data*) *tip*)
    (rplacd (assoc 'polja *send-data*) *polja*)
    (format nil "~S~%" (json:encode-json-to-string *send-data*))))

(defun-ajax drugi (data)(*ajax-processor* :callback-data :response-text)
  (progn
    (json:with-decoder-simple-list-semantics
                  (with-input-from-string     
                      (s data)
                    (simple-json-bind (tip polja) s
                                      (progn
                                        (setq *tip* tip)
                                        (setq *polja* polja)))))
    (setq *data* data)
    (setq *tip* "drugi")
    (setq *polja* (uvecaj *polja*))
    (rplacd (assoc 'tip *send-data*) *tip*)
    (rplacd (assoc 'polja *send-data*) *polja*)
    (format nil "~S~%" (json:encode-json-to-string *send-data*))))

(defparameter _board 0)
(defparameter _player 0)
(defparameter _state 0)
(defparameter _send-data '( (board . 0) (player . 0) (state . 0)))

(defparameter _axis (list "1 2 3 4 5 6 7" "A B C D E F G"))

(defun-ajax example-chess (data)(*ajax-processor* :callback-data :response-text)
  (progn
    (setq *data* data)
    (json:with-decoder-simple-list-semantics
                  (with-input-from-string     
                      (s data)
                    (simple-json-bind (board player state) s
                                      (progn
                                        (setq _board board)
                                        (setq _player player)
                                        (setq _state state)))))
    
    (rplacd (assoc ':type _board) "rectangular")
    (rplacd (assoc ':dimensions _board) '(7 7))
    (rplacd (assoc ':axis _board) _axis)
    (rplacd (assoc 'board _send-data) _board)
    (rplacd (assoc 'player _send-data) _player)
    (rplacd (assoc 'state _send-data) _state)
    
    ;(setq proba (list _board _player _state))
    (format nil "~S~%" (json:encode-json-to-string _send-data))))

    
    



(defun uvecaj (niz)
  (cond 
   ((null niz) '())
   (t (cons (+ 1 (car niz)) (uvecaj (cdr niz))))))





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
  (start (make-instance 'easy-acceptor :address "localhost" :port 8081)))

(setq *dispatch-table* (list 'dispatch-easy-handlers
                            (create-ajax-dispatcher *ajax-processor*)))