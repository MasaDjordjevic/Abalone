;(in-package :jank-repl)

;;; Vraca listu od 6 koordinata tj. susede.
(defun kreirajSusede (cvor)
  (let* ((x (car cvor))
         (y (cadr cvor))
         (z (caddr cvor)))
    (list
     (list (+ x 1) (- y 1) z)
     (list (- x 1) (+ y 1) z)
     (list (+ x 1) y (- z 1))
     (list (- x 1) y (+ z 1))
     (list x (+ y 1) (- z 1))
     (list x (- y 1) (+ z 1)))))

;;; Ispituje da li je EL clan LISTE.
(defun clanp (el lista)
  (cond
   ((null lista) '())
   ((equal el (car lista)) t)
   (t (clanp el (cdr lista)))))


;;; Iz LISTE cvorova izbacuje one cija je bar jedna koordinata veca od GRANICE.
(defun odbaciElementeVanGranica (lista granica)
  (cond
   ((null lista) '())
   (t (let* ((cvor (car lista))
             (x (car cvor))
             (y (cadr cvor))
             (z (caddr cvor)))
        (cond
          ((or (>= (abs x) granica) (>= (abs y) granica) (>= (abs z) granica))
               (odbaciElementeVanGranica (cdr lista) granica))
          (t (cons (car lista) (odbaciElementeVanGranica (cdr lista) granica))))))))


;;; Na LISTU dodaje SUSEDE tako da elementi LISTE ostanu jedinstveni.
(defun dopuniListu (susedi lista)
  (cond ((null susedi) lista )
        ((clanp (car susedi) lista) (dopuniListu (cdr susedi) lista))
        (t (cons (car susedi) (dopuniListu (cdr susedi) lista)))))

;;; Na LISTU dodaje ELEMENTE koji nisu clanovi ni LISTE ni LISTE2
(defun dodajNeobradjene (elementi lista lista2)
  (cond ((null elementi) lista )
        ((or (clanp (car elementi) lista) (clanp (car elementi) lista2))
          (dodajNeobradjene (cdr elementi) lista lista2))
        (t (cons (car elementi) (dodajNeobradjene (cdr elementi) lista lista2)))))


;;; Vraca listu sa koordinatama table odgovarajuce VELICINE.
;;; Treba da se pozove sa NEOBRADJENI = centralni cvor.
;;; OBRADJENI su inicijalno prazni.
;;; (kreirajKoordinate '((0 0 0)) '() 5)
(defun kreirajKoordinate (neobradjeni obradjeni velicina)
  (cond ((null neobradjeni) obradjeni)
        (t (let* ((noviSusedipom (kreirajSusede (car neobradjeni)))
                  (noviSusedi (odbaciElementeVanGranica noviSusedipom velicina))
                  (noviObradjeni (cons (car neobradjeni) obradjeni))
                  (noviNeobradjeni (dodajNeobradjene noviSusedi (cdr neobradjeni) noviObradjeni))
                  ;;(a (format t "noviObradjeni: ~s~%" noviObradjeni))
                  ;;(a (format t "noviNeobradjeni: ~s~%" noviNeobradjeni))
                 )
             (kreirajKoordinate noviNeobradjeni noviObradjeni velicina)))))

;;; Glavna funkcija za kreiranje table.
(defun kreirajTablu (velicina)
  (let* ((k (- velicina 2)) ;
         (-k (- 0 k))       ; zbog citljivosti
         (praznaTabla (kreirajPraznuTablu velicina))
         ;; Kreira gornji levi / donji desni X i sve njegove susede.
         (x1 (append (list (list 0 k -k)) (kreirajSusede (list 0 k -k))))
         (x2 (append (list (list 0 -k k)) (kreirajSusede (list 0 -k k))))
         (x (append x1 x2))
         ;; Kreira gornji desni / donji levi O i sve njegove susede.
         (o1 (append (list (list k 0 -k)) (kreirajSusede (list k 0 -k))))
         (o2 (append (list (list -k 0 k)) (kreirajSusede (list -k 0 k))))
         (o (append o1 o2))
         (tabla (postaviVrednosti x "x" praznaTabla))
         (tabla (postaviVrednosti o "o" tabla)))
    (sortiraj tabla 'opPoredjenja)))

;;; Interfejsna funkcija.
;;; Kreira sve koordinate i pozove pomocnu.
(defun kreirajPraznuTablu(velicina)
  (kreirajPraznutablu1 (kreirajKoordinate '((0 0 0)) '() velicina)))

;;; Pomocna funkcija koja pravi cvorove.
;;; Na svaku koordinatu stavlja crticu.
;;; TABLA je zapravo samo lista koordinata.
(defun kreirajPraznuTablu1 (tabla)
  (cond ((null tabla) '())
        (t (cons (list (car tabla) "-") (kreirajPraznuTablu1 (cdr tabla))))))

(defun postaviVrednost (polje vrednost tabla)
  (cond ((equal (caar tabla) polje) (cons (list (caar tabla) vrednost) (cdr tabla)))
         (t (append (list (car tabla)) (postaviVrednost polje vrednost (cdr tabla))))))

;;; Svim POLJIMA postavlja istu VREDNOST na TABLI.
(defun postaviVrednosti (polja vrednost tabla)
  (cond ((null polja) tabla)
    (t (let* ((novaTabla (postaviVrednost (car polja) vrednost tabla)))
         (postaviVrednosti (cdr polja) vrednost novaTabla)))))

;;; Vraca znak POLJA iz TABLE.
(defun znak (polje tabla)
  (cond ((null tabla) '())
        ((equal (caar tabla) polje) (cadar tabla))
        (t (znak polje (cdr tabla)))))

;;; Poredi po Z koordinati (horizontalno), pa onda po X.
;;; Ocekuje i koordinate i znak.
(defun opPoredjenja (a b)
  (let* ((ax (caar a))
         (az (caddar a))
         (bx (caar b))
         (bz (caddar b)))
    (or (< az bz) (and (= az bz) (< ax bx)))))

;;; Poredi po Z koordinati (horizontalno), pa onda po X.
;;; Ocekuje samo koordinate.
(defun opPoredjenjaKoordSamo (a b)
  (let* ((ax (car a))
         (az (caddr a))
         (bx (car b))
         (bz (caddr b)))
    (or (< az bz) (and (= az bz) (< ax bx)))))

(defun sortiraj (lista op)
  (cond ((null lista) '())
        (t (umetni (car lista) (sortiraj (cdr lista) op) op))))

;;; Pomocna funkcija za sortiranje.
(defun umetni (el lista op)
  (cond ((null lista) (list el))
        ((apply op (list el (car lista))) (cons el lista))
        (t (cons (car lista) (umetni el (cdr lista) op)))))

(defun stampaj (tabla)
  ;; Uzima Z prvog elementa umanjen za jedan.
  (stampaj1 tabla (1- (caddr (caar tabla)))))

;; [ako hoces u cond da dodas stampanje slova (napraviRazmak (abs z)) (format nil "~s" (abs z))]
;;;
(defun stampaj1 (tabla z)
  (cond ((null tabla) '())
        (t (let* ((novoZ (caddr (caar tabla))))
             (cond ((not (= z novoZ))
                      (concatenate 'string  (format nil "~%") (napraviRazmak (abs novoZ)) (cadar tabla) '" " (stampaj1 (cdr tabla) novoZ)))
                   (t (concatenate 'string (cadar tabla) '" " (stampaj1 (cdr tabla) novoZ))))))))

;;; Pomocna funckija za stampanje.
;;; Vraca string od N razmaka.
(defun napraviRazmak (n)
  (cond ((= 0 n) '())
        (t (concatenate 'string '" " (napraviRazmak (- n 1))))))

;;; TESTIRAJ:

;;; Test na ciljno stanje.
;;; Očekuje čvorove.
(defun kraj-p (tabla)
  (or (<= (prebroji "x" tabla) 8) (<= (prebroji "o" tabla) 8)))

;;; Pomoćna funkcija.
(defun prebroji (znak tabla)
  (cond ((null tabla) 0)
        ((equal (cadar tabla) znak) (1+ (prebroji znak (cdr tabla))))
        (t (prebroji znak (cdr tabla)))))
