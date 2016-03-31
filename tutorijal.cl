

(defpackage :jank-repl
  (:use :cl :hunchentoot :cl-who :parenscript :smackjack))
(in-package :jank-repl)

; Allow cl-who and parenscript to work together
(setf *js-string-delimiter* #\")

(defparameter *ajax-processor*
  (make-instance 'ajax-processor :server-uri "/repl-api"))

(defun-ajax echo (data) (*ajax-processor* :callback-data :response-text)
  (let (
        (a (format t "rezultat: ~S~%" (eval data))) 
        (b (format t "data: ~S~%"  data))
        (c (eval data))
        (d (string-to-list data))
        )
    (format nil "rezultat: ~S~%" (eval d) )
    ;;(concatenate 'string "echo: " (eval d))    
    )
  )

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

(define-easy-handler (repl :uri "/repl") ()
  (with-html-output-to-string (s)
    (:html
      (:head
        (:title "Jank REPL")
        (str (generate-prologue *ajax-processor*))
        (:script :type "text/javascript"
          (str
            (ps
              (defun callback (response)
                (alert response))
              (defun on-click ()
                (chain smackjack (echo (chain document
                                              (get-element-by-id "data")
                                              value)
                                       callback)))))))
      (:body
        (:p
          (:input :id "data" :type "text"))
        (:p
          (:button :type "button"
                   :onclick (ps-inline (on-click))
                   "Submit!"))))))

(define-easy-handler (repl :uri "/repla") ()
  (with-html-output-to-string (s)
    (:html
      (:head
        (:title "Jank REPL")
        (str (generate-prologue *ajax-processor*))
        (:script :type "text/javascript"
          (str
            (ps
              (defun callback (response)
                (alert response))
              (defun on-click ()
                (chain smackjack (echo (chain document
                                              (get-element-by-id "data")
                                              value)
                                       callback)))))))
      (:body
        (:p
          (:input :id "data" :type "text"))
        (:p
          (:button :type "button"
                   :onclick (ps-inline (on-click))
                   "Submit!")))
     (:script :type "text/javascript" " 
var saveResponse;
var a = 'tralala';
alert(a);
function myCallbackText(response) {
  alert(response);
}

function myCallbackJSON(response) {
  alert(response.name + ' is '+ response.age + ' years old and was born on a ' +response.dayOfBirth );
}

// calls our Lisp function with the value of the text field
function sayHi() {
  smackjack.sayHi(document.getElementById('name').value, myCallbackXml);
}
function sayBye() {
  smackjack.sayBye(document.getElementById('name').value, myCallbackText);
}
function forceError() {
  smackjack.forceError(document.getElementById('name').value, myCallbackXml);
}


"))

     
     ))


(define-easy-handler (main-page :uri "/masa"
                                :acceptor-names (list 'my-server)) ()
  (with-html-output-to-string (*standard-output* nil :prologue t)
    (:html :xmlns "http://www.w3.org/1999/xhtml"
     (:head
      (:title "smackjack demo")
      (str (generate-prologue *ajax-processor*))
      (:script :type "text/javascript" " 
var saveResponse;

function myCallbackText(response) {
  alert(response);
}

function myCallbackJSON(response) {
  alert(response.name + ' is '+ response.age + ' years old and was born on a ' +response.dayOfBirth );
}

// calls our Lisp function with the value of the text field
function sayHi() {
  smackjack.sayHi(document.getElementById('name').value, myCallbackXml);
}
function sayBye() {
  smackjack.sayBye(document.getElementById('name').value, myCallbackText);
}
function forceError() {
  smackjack.forceError(document.getElementById('name').value, myCallbackXml);
}

}
"))
     (:body
      (:p "Please enter your name: " 
          (:input :id "name" :type "text"))
      (:p (:a :href "javascript:sayHi()" "Say Hi!"))
      (:p (:a :href "javascript:sayBye()" "Say Bye!"))
      ))))

(defparameter *server*
  (start (make-instance 'easy-acceptor :address "localhost" :port 8080)))

(setq *dispatch-table* (list 'dispatch-easy-handlers
                             (create-ajax-dispatcher *ajax-processor*)))


