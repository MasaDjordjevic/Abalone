
(defpackage :jank-repl
  (:use :cl :hunchentoot :cl-who :smackjack))


(in-package :jank-repl)


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

(defparameter _markings 0)

(defparameter _removed 0)
         

(defparameter _send-data '( (board . 0) (player . 0) (state . 0) (markings . 0) (removed . 0)))

(defun reset-paremeters ()
  (progn
    (setq *data* 0)
    (setq _board 0)
    (setq _player 0)
    (setq _state '())
    (setq _markings '())
    (setq _removed '())))


(defun-ajax reset (data)(*ajax-processor* :callback-data :response-text)
  (progn
    (setq *data* data) 
    
    (reset-paremeters)
    
    (setq _board '(
                   (type . "rectangular")
                   (dimensions . (15 15))
                   (corner . "bottom-left")
                   (axis . ("1 2 3 4 5 6 7 8 9 10 11 12 13 14 15" "A B C D E F G H I J K L M N O"))
                   (coloring . "classic")
                   (mode . "classic")
                   (size . "m")))
    
    (setq _player '(
                    (name . "La Plavusha")
                    (order . 1)
                    (message . "Cemu ova poruka"))) 
    
    (rplacd (assoc 'board _send-data) _board)
    (rplacd (assoc 'player _send-data) _player)
    (rplacd (assoc 'state _send-data) _state)    
    (rplacd (assoc 'markings _send-data) _markings)
    (rplacd (assoc 'removed _send-data) _removed)
    (format nil "~S~%" (json:encode-json-to-string _send-data))))

(defun-ajax example-xo (data)(*ajax-processor* :callback-data :response-text)
  (progn
    (setq *data* data)   
    (reset-paremeters)
    
    (setq _board '(
                   (type . "rectangular")
                   (dimensions . (15 15))
                   (corner . "bottom-left")
                   (axis . ("1 2 3 4 5 6 7 8 9 10 11 12 13 14 15" "A B C D E F G H I J K L M N O"))
                   (mode . "classic")
                   (coloring . "classic")
                   (size . "m")))
    (setq _player '(
                    (name . "La Plavusha")
                    (order . 1)
                    (message . "Zdravo deco")))
    (setq _state '(
                   (
                    (fields . (("7" "O")("12" "D")("9" "H")))
                    (style . (
                              (color . "red")
                              (shape . "X"))))
                   (
                    (fields . (("14" "H")("10" "C")("1" "A")))
                    (style . (
                              (color . "blue")
                              (shape . "O"))))))
    (setq _markings '())   
    
    (rplacd (assoc 'board _send-data) _board)
    (rplacd (assoc 'player _send-data) _player)
    (rplacd (assoc 'state _send-data) _state)    
    (rplacd (assoc 'markings _send-data) _markings)
    (rplacd (assoc 'removed _send-data) _removed)  
    
    (format nil "~S~%" (json:encode-json-to-string _send-data))))

(defun-ajax example-gomoku-eastern (data)(*ajax-processor* :callback-data :response-text)
  (progn
    (setq *data* data) 
    (reset-paremeters)
    
    (setq _board '(
                   (type . "rectangular")
                   (dimensions . (15 15))
                   (corner . "bottom-left")
                   (axis . ("1 2 3 4 5 6 7 8 9 10 11 12 13 14 15" "A B C D E F G H I J K L M N O"))
                   (mode . "go")
                   (coloring . "classic")
                   (size . "m")))
    (setq _player '(
                    (name . "La Plávusha")
                    (order . 1)
                    (message . "TutiFruti je awesome")))
    (setq _state '(
                   (
                    (fields . (("13" "C")("12" "D")("11" "E")("11" "F")("11" "G")("11" "H")("11" "J")("11" "L")("12" "M")("10" "F")("10" "G")("10" "I")("10" "K")("9" "F")("9" "H")("9" "J")("9" "L")("8" "E")("8" "F")("8" "G")("8" "H")("8" "M")("7" "E")("7" "H")("6" "D")("6" "E")("6" "I")("6" "L")("5" "E")("5" "H")("5" "K")("4" "F")("4" "J")("3" "D")))
                    (style . (
                              (color . "black")
                              (shape . "circle"))))
                   (
                    (fields . ( ("14" "B")("13" "F")("12" "E")("12" "F")("12" "I")("12" "K")("12" "N")("11" "C")("11" "I")("10" "H")("9" "E")("9" "G")("9" "I")("8" "D")("8" "I")("8" "J")("8" "K")("7" "C")("7" "D")("7" "F")("7" "G")("7" "I")("7" "L")("7" "M")("7" "N")("6" "H")("5" "F")("5" "G")("5" "I")("5" "J")("3" "C")("3" "I")("2" "H")))
                    (style . (
                              (color . "white")
                              (shape . "circle"))))))    
    
    (rplacd (assoc 'board _send-data) _board)
    (rplacd (assoc 'player _send-data) _player)
    (rplacd (assoc 'state _send-data) _state)    
    (rplacd (assoc 'markings _send-data) _markings)
    (rplacd (assoc 'removed _send-data) _removed)  
    
    (format nil "~S~%" (json:encode-json-to-string _send-data))))

(defun-ajax example-gomoku-western (data)(*ajax-processor* :callback-data :response-text)
  (progn
    (setq *data* data)   
    (reset-paremeters)
    
    (setq _board '(
                   (type . "rectangular")
                   (dimensions . (15 15))
                   (corner . "bottom-left")
                   (axis . ("1 2 3 4 5 6 7 8 9 10 11 12 13 14 15" "A B C D E F G H I J K L M N O"))
                   (mode . "classic")
                   (coloring . "classic")
                   (size . "l")))
    (setq _player '(
                    (name . "La Plávusha")
                    (order . 1)
                    (message . "TutiFruti je awesome")))
    (setq _state '(
                   (
                    (fields . (("13" "C")("12" "D")("11" "E")("11" "F")("11" "G")("11" "H")("11" "J")("11" "L")("12" "M")("10" "F")("10" "G")("10" "I")("10" "K")("9" "F")("9" "H")("9" "J")("9" "L")("8" "E")("8" "F")("8" "G")("8" "H")("8" "M")("7" "E")("7" "H")("6" "D")("6" "E")("6" "I")("6" "L")("5" "E")("5" "H")("5" "K")("4" "F")("4" "J")("3" "D")))
                    (style . (
                              (color . "red")
                              (shape . "O"))))
                   (
                    (fields . ( ("14" "B")("13" "F")("12" "E")("12" "F")("12" "I")("12" "K")("12" "N")("11" "C")("11" "I")("10" "H")("9" "E")("9" "G")("9" "I")("8" "D")("8" "I")("8" "J")("8" "K")("7" "C")("7" "D")("7" "F")("7" "G")("7" "I")("7" "L")("7" "M")("7" "N")("6" "H")("5" "F")("5" "G")("5" "I")("5" "J")("3" "C")("3" "I")("2" "H")))
                    (style . (
                              (color . "blue")
                              (shape . "X"))))))
    (rplacd (assoc 'board _send-data) _board)
    (rplacd (assoc 'player _send-data) _player)
    (rplacd (assoc 'state _send-data) _state)    
    (rplacd (assoc 'markings _send-data) _markings)
    (rplacd (assoc 'removed _send-data) _removed)    
    
    (format nil "~S~%" (json:encode-json-to-string _send-data))))

(defun-ajax example-hexa-chess (data)(*ajax-processor*  :callback-data :response-text)
  (progn
    (setq *data* data)   
    (reset-paremeters)
    
    (setq _board '(
                   (type . "hexagonal-pointy")
                   (dimensions . (5 5 6))
                   (corner . "bottom-left")
                   (axis . ("A B C D E F G H I J" "1 2 3 4 5 6 7 8 9"))
                   (mode . "classic")
                   (coloring . "chess")
                   (size . "xxl")))
    (setq _player '(
                    (name . "La Plávusha")
                    (order . 1)
                    (message . "TutiFruti je awesome ?")))
    (setq _state '(
                   (
                    (fields . (("E" "1")("F" "2")("G" "3")("H" "4")("I" "5")("I" "6")("I" "7")("I" "8")("I" "9")))
                    (style . (
                              (color . "black")
                              (shape . "chess-pawn-fill"))))
                   (
                    (fields . (("B" "1")("B" "2")("B" "3")("B" "4")("B" "5")("C" "6")("D" "7")("E" "8")("F" "9")))
                    (style . (
                              (color . "white")
                              (shape . "chess-pawn-fill"))))
                   (
                    (fields . (("F" "1")("J" "9")))
                    (style . (
                              (color . "black")
                              (shape . "chess-rook-fill"))))
                   (
                    (fields . (("H" "3")("J" "8")))
                    (style . (
                              (color . "black")
                              (shape . "chess-knight-fill"))))
                   (
                    (fields . (("G" "2")("J" "7")("I" "4")))
                    (style . (
                              (color . "black")
                              (shape . "chess-bishop-fill"))))
                   (
                    (fields . (("J" "6")))
                    (style . (
                              (color . "black")
                              (shape . "chess-queen-fill"))))
                   (
                    (fields . (("J" "5")))
                    (style . (
                              (color . "black")
                              (shape . "chess-king-fill"))))
                   (
                    (fields . (("A" "1")("E" "9")))
                    (style . (
                              (color . "white")
                              (shape . "chess-rook-fill"))))
                   (
                    (fields . (("A" "2")("C" "7")))
                    (style . (
                              (color . "white")
                              (shape . "chess-knight-fill"))))
                   (
                    (fields . (("A" "3")("B" "6")("D" "8")))
                    (style . (
                              (color . "white")
                              (shape . "chess-bishop-fill"))))
                   (
                    (fields . (("A" "4")))
                    (style . (
                              (color . "white")
                              (shape . "chess-queen-fill"))))
                   (
                    (fields . (("A" "5")))
                    (style . (
                              (color . "white")
                              (shape . "chess-king-fill"))))
                   ))
    (setq _markings '(
                      (
                       (fields . (("F" "5")))
                       (style . (
                                 (color . "pink")
                                 (shape . "star"))))))    
    
    (rplacd (assoc 'board _send-data) _board)
    (rplacd (assoc 'player _send-data) _player)
    (rplacd (assoc 'state _send-data) _state)    
    (rplacd (assoc 'markings _send-data) _markings)
    (rplacd (assoc 'removed _send-data) _removed)   
    
    (format nil "~S~%" (json:encode-json-to-string _send-data))))

(defun-ajax example-chess (data)(*ajax-processor*  :callback-data :response-text)
  (progn
    (setq *data* data)    
    (reset-paremeters)
    
    (setq _board '(
                   (type . "rectangular")
                   (dimensions . (8 8))
                   (corner . "bottom-left")
                   (axis . ("1 2 3 4 5 6 7 8" "A B C D E F G H"))
                   (mode . "classic")
	(coloring . "chess")
                   (size . "xxl")))
    (setq _player '(
                    (name . "La Plávusha")
                    (order . 1)
                    (message . "TutiFruti je awesome ?")))
    (setq _state '(
                   (
                    (fields . (("7" "A")("7" "B")("7" "C")("7" "D")("7" "E")("7" "F")("7" "G")("7" "H")))
                    (style . (
                              (color . "black")
                              (shape . "chess-pawn-fill"))))
                   (
                    (fields . (("2" "A")("2" "B")("2" "C")("2" "D")("2" "E")("2" "F")("2" "G")("2" "H")))
                    (style . (
                              (color . "white")
                              (shape . "chess-pawn-fill"))))
                   (
                    (fields . (("8" "A")("8" "H")))
                    (style . (
                              (color . "black")
                              (shape . "chess-rook-fill"))))
                   (
                    (fields . (("8" "B")("8" "G")))
                    (style . (
                              (color . "black")
                              (shape . "chess-knight-fill"))))
                   (
                    (fields . (("8" "C")("8" "F")))
                    (style . (
                              (color . "black")
                              (shape . "chess-bishop-fill"))))
                   (
                    (fields . (("8" "D")))
                    (style . (
                              (color . "black")
                              (shape . "chess-queen-fill"))))
                   (
                    (fields . (("8" "E")))
                    (style . (
                              (color . "black")
                              (shape . "chess-king-fill"))))
                   (
                    (fields . (("1" "A")("1" "H")))
                    (style . (
                              (color . "white")
                              (shape . "chess-rook-fill"))))
                   (
                    (fields . (("1" "B")("1" "G")))
                    (style . (
                              (color . "white")
                              (shape . "chess-knight-fill"))))
                   (
                    (fields . (("1" "C")("1" "F")))
                    (style . (
                              (color . "white")
                              (shape . "chess-bishop-fill"))))
                   (
                    (fields . (("1" "D")))
                    (style . (
                              (color . "white")
                              (shape . "chess-queen-fill"))))
                   (
                    (fields . (("1" "E")))
                    (style . (
                              (color . "white")
                              (shape . "chess-king-fill"))))
                   ))
    (setq _markings '(
                      (
                       (fields . (("4" "D")("3" "D")))
                       (style . (
                                 (color . "blue")
                                 (shape . "circle"))))
                      (
                       (fields . (("2" "D")))
                       (style . (
                                 (color . "yellow")
                                 (shape . "O"))))))
    
    
    (rplacd (assoc 'board _send-data) _board)
    (rplacd (assoc 'player _send-data) _player)
    (rplacd (assoc 'state _send-data) _state)    
    (rplacd (assoc 'markings _send-data) _markings)
    (rplacd (assoc 'removed _send-data) _removed)    
    
    (format nil "~S~%" (json:encode-json-to-string _send-data))))


(defun-ajax odigraj-potez (data)(*ajax-processor* :method :post :callback-data :response-text)
  (progn
    (setq *data* data)
    (json:with-decoder-simple-list-semantics
                  (with-input-from-string     
                      (s data)
                    (simple-json-bind (board player state markings removed) s
                                      (progn
                                        (setq _board board)
                                        (setq _player player)
                                        (setq _state state)
                                        (setq _markings markings)
                                        (setq _removed removed)))))
    (_odigraj-potez)

    (rplacd (assoc 'board _send-data) _board)
    (rplacd (assoc 'player _send-data) _player)
    (rplacd (assoc 'state _send-data) _state)    
    (rplacd (assoc 'markings _send-data) _markings)
    (rplacd (assoc 'removed _send-data) _removed)
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