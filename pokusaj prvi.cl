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







    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    