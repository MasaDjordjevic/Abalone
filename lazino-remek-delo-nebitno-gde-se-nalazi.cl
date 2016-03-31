(defpackage :jank-repl
  (:use :cl :hunchentoot :cl-who :parenscript :smackjack))


(in-package :jank-repl)



;; vraca listu od 6 koordinata tj. susede
(defun kreirajSusede (cvor)
  (let* (
         (x (car cvor))
         (y (cadr cvor))
         (z (caddr cvor))
         )
    
    (list 
     (list (+ x 1) (- y 1) z)
     (list (- x 1) (+ y 1) z)
     (list (+ x 1) y (- z 1))
     (list (- x 1) y (+ z 1))
     (list x (+ y 1) (- z 1))
     (list x (- y 1) (+ z 1))
     )    
    )        
  )

(defun clanp (el lista)
  (cond 
   ( (null lista) '())
   ( (equal el (car lista)) t)
   (t (clanp el (cdr lista)))
   )
  )


;; iz liste cvorova izbacuje one cija je bar jedna koordinata veca od granice
(defun odbaciElementeVanGranica (lista granica)
  (cond 
   ( (null lista) '())
   ( t 
    (let* (
            (cvor (car lista))
            (x (car cvor))
            (y (cadr cvor))
            (z (caddr cvor))
            )
      (cond 
       ( (or (>= (abs x) granica) (>= (abs y) granica) (>= (abs z) granica) )
        (odbaciElementeVanGranica (cdr lista) granica) 
        )
       (t 
        (cons (car lista) (odbaciElementeVanGranica (cdr lista) granica) ) 
        )   
       )
      )
    )
   )
  )
  

;; na listu dodaje susede tako da elementi liste ostaju jedinstveni
(defun dopuniListu(susedi lista)
  (cond   
   ( (null susedi) lista )   
   ( (clanp (car susedi) lista) (dopuniListu (cdr susedi) lista))
   ( t (cons (car susedi) (dopuniListu (cdr susedi) lista) ))
  )
  )

;; na listu dodaje elmente koji nisu clanovi liste1 ili liste2
(defun dodajNeobradjene(susedi lista lista2)
  (cond   
   ( (null susedi) lista )   
   ( (or (clanp (car susedi) lista) (clanp (car susedi) lista2)) (dodajNeobradjene (cdr susedi) lista lista2))
   ( t (cons (car susedi) (dodajNeobradjene (cdr susedi) lista lista2) ))
  )
  )


;;vraca listu sa koordinatama table odgovarajuce velicine
;; cvor je centralni cvor
;; (kreirajTablu '((0 0 0)) '() 5)
(defun kreirajKoordinate(neobradjeni obradjeni velicina)
  (cond 
   ( (null neobradjeni) obradjeni)   
   ( t
    (let* 
      (       
       (noviSusedipom (kreirajSusede (car neobradjeni)))
       (noviSusedi (odbaciElementeVanGranica noviSusedipom velicina))
       (noviObradjeni (cons (car neobradjeni) obradjeni))
       (noviNeobradjeni (dodajNeobradjene noviSusedi (cdr neobradjeni) noviObradjeni))
     
       ;;(a (format t "noviObradjeni: ~s~%" noviObradjeni))
       ;;(a (format t "noviNeobradjeni: ~s~%" noviNeobradjeni))      
       )
      (kreirajKoordinate noviNeobradjeni noviObradjeni velicina)
      )
    )
   )
  )


(defun kreirajTablu (velicina)
  (let* (
         (k (- velicina 2))
         (-k (- 0 k))
         (praznaTabla (kreirajPraznuTablu velicina))
         (x1 (append (list (list 0 k -k)) (kreirajSusede (list 0 k -k)) ))
         (x2 (append (list (list 0 -k k)) (kreirajSusede (list 0 -k k)) ))
         (x (append x1 x2))
         (o1 (append (list (list k 0 -k)) (kreirajSusede (list k 0 -k)) ))
         (o2 (append (list (list -k 0 k)) (kreirajSusede (list -k 0 k)) ))
         (o (append o1 o2))    
         (tabla (postaviVrednosti x "x" praznaTabla))
         (tabla (postaviVrednosti o "o" tabla))                
         )
    (sortiraj tabla 'opPoredjenja)
    )        
  )

(defun kreirajPraznuTablu(velicina)
  (kreirajPraznutablu1 (kreirajKoordinate '((0 0 0)) '() velicina) ) 

  )

(defun kreirajPraznuTablu1 (tabla)
  (cond
   ( (null tabla) '())
   (t (cons (list (car tabla) "-") (kreirajPraznuTablu1 (cdr tabla))))
   )  
  )

(defun postaviVrednost (polje vrednost tabla)
  (cond
   ( (equal (caar tabla) polje) (cons (list (caar tabla) vrednost) (cdr tabla) ))
   (t (append (list (car tabla)) (postaviVrednost polje vrednost (cdr tabla))))
   )
  )

(defun postaviVrednosti (polja vrednost tabla)
  (cond
   ( (null polja) tabla)
   (t (let* (            
             (novaTabla (postaviVrednost (car polja) vrednost tabla))
             )
        (postaviVrednosti (cdr polja) vrednost novaTabla)
        )
      )
   )  
  )

(defun znak (polje tabla)
  (cond
   ( (null tabla) '())
   ( (equal (caar tabla) polje) (cadar tabla))
   ( t (znak polje (cdr tabla)))
   )
  )


;ocekuje i koordinate i znak
(defun opPoredjenja (a b)
  (let* (
         (ax (caar a))
         (az (caddar a))
         (bx (caar b))
         (bz (caddar b))         
         )
    (or (< az bz) (and (= az bz) (< ax bx)) )    
    )
  )

;uzima samo koordinte
(defun opPoredjenjaKoordSamo (a b)
  (let* (
         (ax (car a))
         (az (caddr a))
         (bx (car b))
         (bz (caddr b))         
         )
    (or (< az bz) (and (= az bz) (< ax bx)) )    
    )
  )

(defun sortiraj (l op)
  (cond 
   ((null l) '())
   (t (umetni (car l) (sortiraj (cdr l) op) op))
   )
  )

(defun umetni (el l op)
  (cond ((null l) (list el))
        ((apply op (list el (car l))) (cons el l))
        (t (cons (car l) (umetni el (cdr l) op)))))

(defun stampaj (tabla)
  (stampaj1 tabla (- (caddr (caar tabla)) 1))   
  )

(defun stampaj1 (tabla z)
  (cond 
   ( (null tabla) '())
   (t 
    (let* (
           (novoZ (caddr (caar tabla)))           
         )
    (cond ;; ako hoces dole da dodas stampanje slova (napraviRazmak (abs z)) (format nil "~s" (abs z))
     ( (not (= z novoZ)) (concatenate 'string  (format nil "~%") (napraviRazmak (abs novoZ)) (cadar tabla) '" " (stampaj1 (cdr tabla) novoZ) ))
     (t (concatenate 'string (cadar tabla) '" " (stampaj1 (cdr tabla) novoZ) ))
     )
      )
    )
   )
  )

(defun napraviRazmak(n)
  (cond 
   ( (= 0 n) '())
   ( t (concatenate 'string '" " (napraviRazmak (- n 1))))
   )
  )







    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    


(defun smer (n)
  (cond
   ( (= n 1) '(-1 1 0)) ;levo
   ( (= n 2) '(0 1 -1)) ;gore-levo
   ( (= n 3) '(1 0 -1)) ;gore-desno
   ( (= n 4) '(1 -1 0)) ;desno
   ( (= n 5) '(0 -1 1)) ;dole-desno
   ( (= n 6) '(-1 0 1)) ;dole-levo   
   )
  )

(defun saberiPokomponentno (a b)
  (cond 
   ( (or (null a) (null b)) '())
   (t (cons (+ (car a) (car b)) (saberiPokomponentno (cdr a) (cdr b))))
   )
  )

(defun pomeri-kamen (kamen smer)
  (cond
   ( (= smer 1) (saberiPokomponentno kamen '(-1 1 0))) ;levo
   ( (= smer 2) (saberiPokomponentno kamen '(0 1 -1))) ;gore-levo
   ( (= smer 3) (saberiPokomponentno kamen '(1 0 -1))) ;gore-desno
   ( (= smer 4) (saberiPokomponentno kamen '(1 -1 0))) ;desno
   ( (= smer 5) (saberiPokomponentno kamen '(0 -1 1))) ;dole-desno
   ( (= smer 6) (saberiPokomponentno kamen '(-1 0 1))) ;dole-levo   
   )
  )

;vraca poziciju na kojoj ce kameni da se nalaze ako se pomere u tom smeru
(defun novaPozicija (kameni smer)
  (mapcar (lambda (x) (saberiPokomponentno  x (smer smer))) kameni)
  )

;proverava da se kameni nalaze u linij po nekoj osi tj koordinati
(defun linijaX-p (kameni koord)
  (cond
   ( (null (cdr kameni)) 't)
   ( (= (nth koord (car kameni)) (nth koord (cadr kameni))) (linijaX-p (cdr kameni) koord))
   ( t '())   
   )
  )

;proverava da li se kameni nalaze u linij
(defun linija-p (kameni)
  (or (linijaX-p kameni 0) (linijaX-p kameni 1) (linijaX-p kameni 2))
  )

;vraca broj koji oznacava po kojoj osi su kameni poredjani
(defun linija (kameni)
  (cond
   ((linijaX-p kameni 0) '0)
   ((linijaX-p kameni 1) '1)
   ((linijaX-p kameni 2) '2)
   (t '())
   )
  )

;proverava da li su kameni susedni
(defun susedna2-p (a b)
  (or (equal a (saberiPokomponentno b (smer 1)))
      (equal a (saberiPokomponentno b (smer 2)))
      (equal a (saberiPokomponentno b (smer 3)))
      (equal a (saberiPokomponentno b (smer 4)))
      (equal a (saberiPokomponentno b (smer 5)))
      (equal a (saberiPokomponentno b (smer 6)))
      )
  )

;proverava da li su svi kameni iz liste susedi
(defun susedni-p (kameni)
  (cond
   ( (null (cdr kameni)) 't)
   ( (susedna2-p (car kameni) (cadr kameni)) (susedni-p (cdr kameni))) 
   ( t '())   
   )
  )

;proverava da li su dva cvora istog znaka
(defun istogznaka2-p (a b)
  (equal (cadr a) (cadr b))
  )

;; znak je xo, da li su svi kamenovi x ili svi o
(defun istogznaka-p (kameni)  
  (cond
   ( (null (cdr kameni)) 't)
   ( (istogznaka2-p (car kameni) (cadr kameni)) (istogznaka-p (cdr kameni))) 
   ( t '())   
   )
  )

; proverava da li je legalna pozicija na tabli
(defun natabliJedan-p (pozicija velicina)
  (and 
   (not (member '() (mapcar (lambda (x) (<= (abs x) (1- velicina))) pozicija)))
   (= 0 (+ (car pozicija) (cadr pozicija) (caddr pozicija)))
   )   
  )

 
; proverava da li su sve pozicije legalne
(defun natabli-p (pozicije velicina)
  (cond 
   ( (null pozicije) '())
   ( (null (cdr pozicije)) 't)
   ( (and (natabliJedan-p  (car pozicije) velicina) (natabli-p (cdr pozicije) velicina)))
   )
  )



;;ocekuje sortiranu tablu
(defun velicinaTable (tabla)
  (+ 1 (abs (caddr (caar tabla))))  
  )

; proverava da li su kameni poredjani tako da ako se krecu u smeru smer mogu da guraju
; smer suprotan od pralelnog kretanja
(defun smerIzguravanja-p (kameni smer)
  (let* (
         (linija (linija kameni)) ; 0 po x osi, 1 po y, 2 po z
         )
    (or
     (and (= linija 0) (or (= smer 2) (= smer 5))) 
     (and (= linija 1) (or (= smer 3) (= smer 6))) 
     (and (= linija 2) (or (= smer 1) (= smer 4))) 
     )
    )
  )

; vraca jedan karakter koji oznacava da li se na tom skupu cvorova nalazi
; samo x, samo o, samo -, ili nesto mesano
; pozicija je niz koorindata
(defun staSeNalazi (pozicija)
  (cond
   ( (= 1 (length pozicija)) "m")
   ( (not (istogznaka-p pozicija)) "m")
   (t (cadar pozicija))
    )
  )


;;ocekuje novu poziciju (ne mora da znaci)
;; vraca onaj kamen "koji gura"
;;podrazumeva sortirane kamencice
(defun polje-u-smeru-izguravanja (kameni smer)
  (cond
   ( (member smer '(1 2 3)) (car kameni))
   ( (member smer '(4 5 6)) (car (reverse kameni)))
   (t '())
   )
  )

;; treba da se obrise ali nisam smela
;;podrazumeva sortirane kamencice
;;(defun znak-polja-u-smeru-izguravanja (kameni smer)
 ;; (cadr (polje-u-smeru-izguravanja kameni smer))
 ;; )

;;  -oo xxx
;;  -o xx

;; xx o-

;;usi = u smeru izguravanja 
; vraca listu od tri elementa
; vraca u obliku ( "x" "o" "-")
(defun nadji-susedna-n-usi (kamen smer tabla n)
  (cond
   ( (= 0 n) '())
   (t
    (let* (
           (noviKamen (pomeri-kamen kamen smer))           
           )
      (cons (znak noviKamen tabla) (nadji-susedna-n-usi noviKamen smer tabla (- n 1)))
      )
    )
   )
  )

;;usi = u smeru izguravanja 
; vraca listu od tri elementa
; vraca cvorove
(defun nadji-susedna-n-usi-koord (kamen smer tabla n)
  (cond
   ( (= 0 n) '())
   (t
    (let* (
           (noviKamen (pomeri-kamen kamen smer))           
           )
      (cons (list noviKamen (znak noviKamen tabla)) (nadji-susedna-n-usi-koord noviKamen smer tabla (- n 1)))
      )
    )
   )
  )


;; kameni koji igraju potez, smer u kome se krecu, tabla na kojoj se nalaze 
(defun moguce-guranje-p (kameni smer tabla)
  (let* (
         (prviKamen (polje-u-smeru-izguravanja kameni smer))
         (brojKamena (length kameni))
         (susedi (nadji-susedna-n-usi prviKamen smer tabla brojKamena))
         (suprotanZnak (if (equal (znak (car kameni) tabla) "x") "o" "x"))
         
         (a (format t "prviKamen: ~s~%brojKamena: ~s~%susedi: ~s~%suprotanZnak: ~s~%" prviKamen brojKamena susedi suprotanZnak))
         )
    (cond
     ( (and (equal (car susedi) suprotanZnak) (member (cadr susedi) '("-" '()))) 't)
     ( (and (= 3 brojKamena) (equal (car susedi) suprotanZnak) (equal (cadr susedi) suprotanZnak) (member (caddr susedi) '("-" '()))) 't)
     (t '())
     )
    )
  )


(defun legalanUnos-p (kameni tabla)
  (and    
   (natabli-p kameni (velicinaTable tabla))
   (istogznaka-p (pozicije-u-cvorove kameni tabla))
   (susedni-p kameni) 
   (linija-p kameni) 
   (<= (length kameni) 3)
   )  
  )

;transformise (0 4 -4) u ((0 4 -4) "x")
(defun pozicije-u-cvorove (kameni tabla)
  (mapcar (lambda (x) (append (list x) (list (znak x tabla)))) kameni)
  )

;trasformise ((0 4 -4) "x") u (0 4 -4)
(defun cvorovi-u-pozicije (cvorovi)
  (mapcar 'car cvorovi)
  )



(defun legalanPotez (kameni smer tabla)
  (cond  
   ( (not (legalanUnos-p kameni tabla)) '()) 
   ( (let* (
            (novaPoz (novaPozicija kameni smer))
            (novaPoz (sortiraj novaPoz 'opPoredjenjaKoordSamo))
            (smerIzguravanja (smerIzguravanja-p kameni smer))
            (mojZnak (znak (car kameni) tabla))
            ;(a (format t "novaPoz: ~s~%smerIzguravanja: ~s~%mojZnak: ~s~%" novaPoz smerIzguravanja mojZnak))
            )
       (cond
        ( (not (natabli-p novaPoz (velicinaTable tabla))) '()) ;nova pozicija je van table       
        ( (and (not smerIzguravanja) (equal "-" (staSeNalazi (pozicije-u-cvorove novaPoz tabla)))) t) ;ukoliko nije izguravanje mora da se pomeri na prazno polje         
        ( (and smerIzguravanja (equal "-" (znak (polje-u-smeru-izguravanja novaPoz smer) tabla) )) t) ;mozes da se pomeris na prazno polje u smeru izguravanja
        ( (and smerIzguravanja (equal mojZnak (znak (polje-u-smeru-izguravanja novaPoz smer) tabla)) ) '()) ;ne mozes da se pomeris ako si ti tamo
        
        ( (and smerIzguravanja (moguce-guranje-p kameni smer tabla)) 't) ;ako je moguce guranje moguc je i potez    
        ;( t (format t "dovde"))
        ( t '())
        )
       )
    )   
   )  
  )

;sluzi za paralelno pomeranje
;kameni se pomeraju na prazno polje
(defun potezZamena (staraPozicija novaPozicija tabla mojZnak)
  (postaviVrednosti staraPozicija "-" (postaviVrednosti novaPozicija mojZnak tabla))
  )

;usg= u smeru izguravanja
;ne gura, samo se pomeri za jedan
(defun potezPomerajUSI (staraPozicija novaPozicija tabla mojZnak)
  (postaviVrednosti novaPozicija mojZnak (postaviVrednosti staraPozicija "-" tabla))
  )

(defun potezGuranje (staraPozicija novaPozicija tabla smer mojZnak)
  (let* (
         (prviKamen (polje-u-smeru-izguravanja staraPozicija smer))
         (brojKamena (length staraPozicija))
         (susedi (nadji-susedna-n-usi-koord prviKamen smer tabla brojKamena))
         (susedi (sredi-susede-pom susedi))
         (susedi (cvorovi-u-pozicije susedi))
         (tabla (potezPomerajUSI susedi (novaPozicija susedi smer) tabla (kontra-znak mojZnak))) 
        
         (tabla (potezPomerajUSI staraPozicija novaPozicija tabla mojZnak))
         )
         tabla
    )     

    )
  
  
;od pozadi skida - sve dok ne dodje do nekog znaka
;ostavlaj samo listu suseda koji su nekog znako, eleminise sve - i polja van table
(defun sredi-susede-pom (susedi)
  (cond
   ( (null susedi) '())
   ( (not (clanp (cadar susedi) '( "-" '())   )) (cons (car susedi) (sredi-susede-pom (cdr susedi))))
   ( t '())
   )
  )

(defun kontra-znak (znak)
  (if (equal znak "x") "o" "x")
  )




(defun odigrajPotez (kameni smer tabla)
   (cond  
   ( (not (legalanUnos-p kameni tabla)) '()) 
    ( (let* (
            ;(a (format t "kameni: ~s~%   smer: ~s~%" kameni smer))
            (novaPoz (novaPozicija kameni smer))
            (novaPoz (sortiraj novaPoz 'opPoredjenjaKoordSamo))
            (kameni (sortiraj kameni 'opPoredjenjaKoordSamo))
            (smerIzguravanja (smerIzguravanja-p kameni smer))
            (mojZnak (znak (car kameni) tabla))
            ;(b (format t "novaPoz: ~s~%smerIzguravanja: ~s~%mojZnak: ~s~%" novaPoz smerIzguravanja mojZnak))
            )
       (cond
        ( (not (natabli-p novaPoz (velicinaTable tabla))) '()) ;nova pozicija je van table       
        ( (and (not smerIzguravanja) (equal "-" (staSeNalazi (pozicije-u-cvorove novaPoz tabla)))) (potezZamena kameni novaPoz tabla mojZnak)) ;ukoliko nije izguravanje mora da se pomeri na prazno polje         
        ( (and smerIzguravanja (equal "-" (znak (polje-u-smeru-izguravanja novaPoz smer) tabla) )) (potezPomerajUSI kameni novaPoz tabla mojZnak)) ;mozes da se pomeris na prazno polje u smeru izguravanja
        ( (and smerIzguravanja (equal mojZnak (znak (polje-u-smeru-izguravanja novaPoz smer) tabla)) ) '()) ;ne mozes da se pomeris ako si ti tamo
        
        ( (and smerIzguravanja (moguce-guranje-p kameni smer tabla)) (potezGuranje kameni novaPoz tabla smer mojZnak)) ;ako je moguce guranje moguc je i potez    
        ;( t (format t "dovde"))
        ( t '())
        )
       )
    )   
   ) 
  
  )










; Allow cl-who and parenscript to work together
(setf *js-string-delimiter* #\")

(defparameter *ajax-processor*
  (make-instance 'ajax-processor :server-uri "/repl-api"))

(defun-ajax echo (data) (*ajax-processor* :callback-data :response-text)
  (let (
        ;(a (format t "rezultat: ~S~%" (eval data))) 
        ;(b (format t "data: ~S~%"  data))
        ;(c (eval data))
        (d (string-to-list data))
        )
    (format nil "~S~%" (stampaj
                        (setq *tabla*
                              (if (null (odigrajPotez (car d) (cadr d) *tabla*))
                                  *tabla*
                                  (odigrajPotez (car d) (cadr d) *tabla*)
                                  )
                            )
                        )
      )
    ;;(concatenate 'string "echo: " (eval d))    
    )
  )

(defun-ajax reset (data) (*ajax-processor* :callback-data :response-text)
  (format nil "~S~%" (stampaj (setq *tabla* (kreirajTablu 5))))
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



(defparameter *server*
  (start (make-instance 'easy-acceptor :address "localhost" :port 8080)))

(setq *dispatch-table* (list 'dispatch-easy-handlers
                             (create-ajax-dispatcher *ajax-processor*)))


(defparameter *tabla*
  (kreirajTablu 5))





