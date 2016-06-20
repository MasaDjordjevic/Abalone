
(defpackage :jank-repl
  (:use :cl :hunchentoot :cl-who :smackjack))


(in-package :jank-repl)

; Allow cl-who and parenscript to work together
;(setf *js-string-delimiter* #\")

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


(defparameter *data* 0)

(defparameter _board 0)
                       
(defparameter _player 0)

(defparameter _state 0)
         

(defparameter _send-data '( (board . 0) (player . 0) (state . 0)))

(defun-ajax reset (data)(*ajax-processor* :callback-data :response-text)
  (progn
    (setq *data* data)    
    
    (setq _board '(
                   (type . "rectangular")
                   (dimensions . (15 15))
                   (corner . "bottom-left")
                   (axis . ("1 2 3 4 5 6 7 8 9 10 11 12 13 14 15" "A B C D E F G H I J K L M N O"))
                   (coloring . "classic")
                   (mode . "classic")))
    
    (setq _player '(
                    (name . "La Plavusha")
                    (order . 1)
                    (message . "Cemu ova poruka")))
    
    (setq _state '())
    
    (rplacd (assoc 'board _send-data) _board)
    (rplacd (assoc 'player _send-data) _player)
    (rplacd (assoc 'state _send-data) _state)    

    (format nil "~S~%" (json:encode-json-to-string _send-data))))


(defun-ajax example-chess (data)(*ajax-processor* :method :post :callback-data :response-text)
  (progn
    (setq *data* data)    
    
    (setq _board '(
                   (type . "rectangular")
                   (dimensions . (15 15))
                   (corner . "bottom-left")
                   (axis . ("1 2 3 4 5 6 7 8 9 10 11 12 13 14 15" "A B C D E F G H I J K L M N O"))
                   (coloring . "classic")
                   (mode . "classic")))
    
    (setq _player '(
                    (name . "La Plavusha")
                    (order . 1)
                    (message . "Cemu ova poruka")))
    
    (setq _state '(
                   ( 
                    (fields . ( ("13" "C") ("12" "D") ("11" "E") ("11" "G")))
                    (style . ( 
                              (color . "red")
                              (shape . "X"))))
                   ( 
                    (fields . ( ("14" "B") ("13" "F") ("12" "E") ("12" "F")))
                    (style . ( 
                              (color . "blue")
                              (shape . "O"))))))
    
    (rplacd (assoc 'board _send-data) _board)
    (rplacd (assoc 'player _send-data) _player)
    (rplacd (assoc 'state _send-data) _state)    

    (format nil "~S~%" (json:encode-json-to-string _send-data))))


(defun-ajax odigraj-potez (data)(*ajax-processor* :method :post :callback-data :response-text)
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
    (_odigraj-potez)

    (rplacd (assoc 'board _send-data) _board)
    (rplacd (assoc 'player _send-data) _player)
    (rplacd (assoc 'state _send-data) _state)
    (format nil "~S~%" (json:encode-json-to-string _send-data))))

(defparameter _x 0)
(defparameter _o 0)

(defun create-state ()
  (let* (
         (x (mapcar (lambda (el) (list (write-to-string (car el)) (write-to-string (cadr el)))) _x))         
         (x (list (append (list 'fields) x) '(style . ((color . "red") (shape . "X"))))) 
         (o (mapcar (lambda (el) (list (write-to-string (car el)) (write-to-string (cadr el)))) _o))         
         (o (list (append (list 'fields) o) '(style . ((color . "blue") (shape . "O"))))))
    (list x o)))

(defun read-state()
  (let* (    
         (f (cdr (assoc ':fields (car _state))))
         (x (mapcar (lambda (el) (list (read-from-string (car el)) (read-from-string (cadr el)))) f))
         (f (cdr (assoc ':fields (cadr _state))))
         (o (mapcar (lambda (el) (list (read-from-string (car el)) (read-from-string (cadr el)))) f)))
    (progn
      (setq _x x)
      (setq _o o))))  

(defun _odigraj-potez ()
  (read-state)    
  (pomeri) ;odigravanje poteza
  (setq _state (create-state)))

(defun pomeri ()
  (setq _x (uvecaj-koord _x)))

   
(defun uvecaj-koord (koord)
  (cond
   ((null koord) '())
   (t (cons (list (1- (caar koord)) (cadar koord)) (uvecaj-koord (cdr koord))))))





(defparameter *server*
  (start (make-instance 'easy-acceptor :address "localhost" :port 8081)))

(setq *dispatch-table* (list 'dispatch-easy-handlers
                            (create-ajax-dispatcher *ajax-processor*)))