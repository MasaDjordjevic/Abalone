(defpackage :jank-repl
  (:use :cl :hunchentoot :cl-who :parenscript :smackjack))


(in-package :jank-repl)


;;; Vraca listu od 6 koordinata tj. susede.
(defun kreiraj-susede (cvor)
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
(defun odbaci-elemente-van-granica (lista granica)
  (cond
   ((null lista) '())
   (t (let* ((cvor (car lista))
             (x (car cvor))
             (y (cadr cvor))
             (z (caddr cvor)))
        (cond
          ((or (>= (abs x) granica) (>= (abs y) granica) (>= (abs z) granica))
               (odbaci-elemente-van-granica (cdr lista) granica))
          (t (cons (car lista) (odbaci-elemente-van-granica (cdr lista) granica))))))))


;;; Na LISTU dodaje SUSEDE tako da elementi LISTE ostanu jedinstveni.
;;; Ova funkcija se nigde ne poziva.
(defun dopuni-listu (susedi lista)
  (cond ((null susedi) lista )
        ((clanp (car susedi) lista) (dopuni-listu (cdr susedi) lista))
        (t (cons (car susedi) (dopuni-listu (cdr susedi) lista)))))

;;; Na LISTU dodaje ELEMENTE koji nisu clanovi ni LISTE ni LISTE2
(defun dodaj-neobradjene (elementi lista lista2)
  (cond ((null elementi) lista )
        ((or (clanp (car elementi) lista) (clanp (car elementi) lista2))
          (dodaj-neobradjene (cdr elementi) lista lista2))
        (t (cons (car elementi) (dodaj-neobradjene (cdr elementi) lista lista2)))))


;;; Vraca listu sa koordinatama table odgovarajuce VELICINE.
;;; Treba da se pozove sa NEOBRADJENI = centralni cvor.
;;; OBRADJENI su inicijalno prazni.
;;; (kreiraj-koordinate '((0 0 0)) '() 5)
(defun kreiraj-koordinate (neobradjeni obradjeni velicina)
  (cond ((null neobradjeni) obradjeni)
        (t (let* ((noviSusedipom (kreiraj-susede (car neobradjeni)))
                  (noviSusedi (odbaci-elemente-van-granica noviSusedipom velicina))
                  (noviObradjeni (cons (car neobradjeni) obradjeni))
                  (noviNeobradjeni (dodaj-neobradjene noviSusedi (cdr neobradjeni) noviObradjeni))
                  ;;(a (format t "noviObradjeni: ~s~%" noviObradjeni))
                  ;;(a (format t "noviNeobradjeni: ~s~%" noviNeobradjeni))
                 )
             (kreiraj-koordinate noviNeobradjeni noviObradjeni velicina)))))

;;; Glavna funkcija za kreiranje table.
(defun kreiraj-tablu (velicina)
  (let* (

         (k (- velicina 2)) ;
         (-k (- 0 k))       ; zbog citljivosti
         (praznaTabla (kreiraj-praznu-tablu velicina))
         ;; Kreira gornji levi / donji desni X i sve njegove susede.
         (x1 (append (list (list 0 k -k)) (kreiraj-susede (list 0 k -k))))
         (x2 (append (list (list 0 -k k)) (kreiraj-susede (list 0 -k k))))
         (x (append x1 x2))

         ;; Kreira gornji desni / donji levi O i sve njegove susede.
         (o1 (append (list (list k 0 -k)) (kreiraj-susede (list k 0 -k))))
         (o2 (append (list (list -k 0 k)) (kreiraj-susede (list -k 0 k))))
         (o (append o1 o2))

         (tabla (postavi-inicijalne-vrednosti x "x" praznaTabla))
         (tabla (postavi-inicijalne-vrednosti o "o" tabla)))

    (sortiraj tabla 'op-poredjenja)))

;;; Interfejsna funkcija.
;;; Kreira sve koordinate i pozove pomocnu.
(defun kreiraj-praznu-tablu(velicina)
  (kreiraj-praznu-tablu1 (kreiraj-koordinate '((0 0 0)) '() velicina)))

;;; Pomocna funkcija koja pravi cvorove.
;;; Na svaku koordinatu stavlja crticu.
;;; TABLA je zapravo samo lista koordinata.
(defun kreiraj-praznu-tablu1 (tabla)
  (cond ((null tabla) '())
        (t (cons (list (car tabla) "-") (kreiraj-praznu-tablu1 (cdr tabla))))))

(defun postaviVrednost (polje vrednost tabla)
  (cond ((equal (caar tabla) polje) (cons (list (caar tabla) vrednost) (cdr tabla)))
         (t (append (list (car tabla)) (postaviVrednost polje vrednost (cdr tabla))))))

;;; Brise nelegalne pozicije iz POZICIJE
(defun izbaci-nelegalne (pozicije velicina)
  (cond ((null pozicije) '())
        ((cond ((natabli1-p  (car pozicije) velicina) (cons (car pozicije) (izbaci-nelegalne (cdr pozicije) velicina)))
               (t (izbaci-nelegalne (cdr pozicije) velicina))))))

;;; Svim POLJIMA postavlja istu VREDNOST na TABLI.
;;; Kopirano je iz leta u drugom null, moze da se sredi!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
(defun postavi-vrednosti (polja vrednost tabla)
  (cond ((null polja) tabla)
        ((null (izbaci-nelegalne polja (velicina-table tabla))) tabla)
        (t (let* ((polja (izbaci-nelegalne polja (velicina-table tabla)))
                  (novaTabla (postaviVrednost (car polja) vrednost tabla)))
             (postavi-vrednosti (cdr polja) vrednost novaTabla)))))

;;; Svim POLJIMA postavlja istu VREDNOST na TABLI.
;;; funkcija postavi-vrednosti ne radi kad se inicijalizuje tabla, nemamo snage da provalimo zasto!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
(defun postavi-inicijalne-vrednosti (polja vrednost tabla)
  (cond ((null polja) tabla)
        (t (let* ((novaTabla (postaviVrednost (car polja) vrednost tabla)))
         (postavi-inicijalne-vrednosti (cdr polja) vrednost novaTabla)))))

;;; Vraca znak POLJA iz TABLE.
(defun znak (polje tabla)
  (cond ((null tabla) '())
        ((equal (caar tabla) polje) (cadar tabla))
        (t (znak polje (cdr tabla)))))

;;; Poredi po Z koordinati (horizontalno), pa onda po X.
;;; Ocekuje i koordinate i znak.
(defun op-poredjenja (a b)
  (let* ((ax (caar a))
         (az (caddar a))
         (bx (caar b))
         (bz (caddar b)))
    (or (< az bz) (and (= az bz) (< ax bx)))))

;;; Poredi po Z koordinati (horizontalno), pa onda po X.
;;; Ocekuje samo koordinate.
(defun op-poredjenja-koord-samo (a b)
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

;; [ako hoces u cond da dodas stampanje slova (napravi-razmak (abs z)) (format nil "~s" (abs z))]
;;;
(defun stampaj1 (tabla z)
  (cond ((null tabla) '())
        (t (let* ((novoZ (caddr (caar tabla))))
             (cond ((not (= z novoZ))
                      (concatenate 'string  (format nil "~%") (napravi-razmak (abs novoZ)) (cadar tabla) '" " (stampaj1 (cdr tabla) novoZ)))
                   (t (concatenate 'string (cadar tabla) '" " (stampaj1 (cdr tabla) novoZ))))))))

;;; Pomocna funckija za stampanje.
;;; Vraca string od N razmaka.
(defun napravi-razmak (n)
  (cond ((= 0 n) '())
        (t (concatenate 'string '" " (napravi-razmak (- n 1))))))

;;; TESTIRAJ:

;;; Test na ciljno stanje.
;;; Ocekuje cvorove.
(defun kraj-p (tabla)
  (or (<= (prebroji "x" tabla) 8) (<= (prebroji "o" tabla) 8)))

;;; Pomocna funkcija.
(defun prebroji (znak tabla)
  (cond ((null tabla) 0)
        ((equal (cadar tabla) znak) (1+ (prebroji znak (cdr tabla))))
        (t (prebroji znak (cdr tabla)))))























(defconstant _desno 1)
(defconstant _goredesno 2)
(defconstant _gorelevo 3)
(defconstant _levo 4)
(defconstant _dolelevo 5)
(defconstant _doledesno 6)


(defun smer (n)
  (cond ((= n _levo) '(-1 1 0))   ; levo
        ((= n _gorelevo) '(0 1 -1))   ; gore-levo
        ((= n _goredesno) '(1 0 -1))   ; gore-desno
        ((= n _desno) '(1 -1 0))   ; desno
        ((= n _doledesno) '(0 -1 1))   ; dole-desno
        ((= n _dolelevo) '(-1 0 1)))) ; dole-levo

;;; Ocekuje koordinate
;;; Npr: (0 1 -1) + (2 -2 0) = (2 -1 -1).
(defun saberi-pokomponentno (a b)
  (cond ((or (null a) (null b)) '())
        (t (cons (+ (car a) (car b)) (saberi-pokomponentno (cdr a) (cdr b))))))

(defun pomeri-kamen (kamen smer)
  (saberi-pokomponentno kamen (smer smer)))

;;; Vraca poziciju na kojoj ce KAMENI da se nalaze ako se pomere u datom SMERU.
(defun nova-pozicija (kameni smer)
  (mapcar (lambda (x) (saberi-pokomponentno  x (smer smer))) kameni))

;;; Pomocna funkcija.
;;; Proverava da se KAMENI nalaze u liniji po zadatoj osi tj. KOORDINATI.
(defun linijaX-p (kameni koord)
  (cond ((null (cdr kameni)) 't)
        ((= (nth koord (car kameni)) (nth koord (cadr kameni))) (linijaX-p (cdr kameni) koord))
        (t '())))

;;; Proverava da li se KAMENI nalaze u bilo kojoj liniji.
(defun linija-p (kameni)
  (or (linijaX-p kameni 0) (linijaX-p kameni 1) (linijaX-p kameni 2)))

;;; Vraca broj koji oznacava po kojoj osi su KAMENI poredjani.
(defun linija (kameni)
  (cond ((linijaX-p kameni 0) '0) ; x
        ((linijaX-p kameni 1) '1) ; y
        ((linijaX-p kameni 2) '2) ; z
        (t '())))

;;; Proverava da li su kameni A i B susedi.
(defun susedna2-p (a b)
  (or (equal a (saberi-pokomponentno b (smer 1)))
      (equal a (saberi-pokomponentno b (smer 2)))
      (equal a (saberi-pokomponentno b (smer 3)))
      (equal a (saberi-pokomponentno b (smer 4)))
      (equal a (saberi-pokomponentno b (smer 5)))
      (equal a (saberi-pokomponentno b (smer 6)))))

;;; Proverava da li su svi KAMENI susedi.
(defun susedni-p (kameni)
  (cond ((null (cdr kameni)) 't)
        ((susedna2-p (car kameni) (cadr kameni)) (susedni-p (cdr kameni)))
        (t '())))

;;; Proverava da li su dva cvora A i B istog znaka (X, O).
(defun istogznaka2-p (a b) (equal (cadr a) (cadr b)))

;;; Da li su svi KAMENI X ili svi O.
(defun istogznaka-p (kameni)
  (cond ((null (cdr kameni)) 't)
        ((istogznaka2-p (car kameni) (cadr kameni)) (istogznaka-p (cdr kameni)))
        (t '())))

;;; Proverava da li je KOORDINATA na tabli velicine VELICINA legalna.
(defun natabli1-p (koord velicina)
  ;; Za sve tri komponente koordinate proverava da li su po apsolutnoj vrednosti
  ;; manje od velicina - 1. Zatim proverava da je zbir komponenata jednak nuli.
  (and (not (member '() (mapcar (lambda (x) (<= (abs x) (1- velicina))) koord)))
       (= 0 (+ (car koord) (cadr koord) (caddr koord)))))

;;; Proverava da li su sve prosledjene POZICIJE legalne.
(defun natabli-p (pozicije velicina)
  (cond ((null pozicije) '())
        ((null (cdr pozicije)) 't)
        ((and (natabli1-p  (car pozicije) velicina) (natabli-p (cdr pozicije) velicina)))))



;;; Ocekuje sortiranu tablu.
(defun velicina-table (tabla) (1+ (abs (caddr (caar tabla)))))

;;; Smer guranja = smer u pravcu u kom su poredjani kamencici
;;; (nije smer paralelnog kretanja).
;;; Proverava da li su KAMENI poredjani tako da, ako se krecu u zadatom SMERU,
;;; mogu da guraju.
(defun smer-guranja-p (kameni smer)
  ;; Linija je: 0 po x osi, 1 po y, 2 po z.
  (let* ((linija (linija kameni)))
    (or (and (= linija 0) (or (= smer _gorelevo) (= smer _doledesno)))    ; x
        (and (= linija 1) (or (= smer _goredesno) (= smer _dolelevo)))    ; y
        (and (= linija 2) (or (= smer _levo) (= smer _desno))))))         ; z

;;; Vraca jedan karakter koji oznacava da li se na tom skupu CVOROVA nalazi:
;;; samo x, samo o, samo -, ili nesto mesano 'm'
;;; pozicija je niz koorindata
(defun sta-se-nalazi (cvorovi)
  (cond ((not (istogznaka-p cvorovi)) "m")
        (t (cadar cvorovi)))) ; ako je stigao dovde, znaci da su svi istog znaka,
                              ; pa vraca znak prvog cvora

;;; Vraca onaj kamen "koji gura".
;;; Podrazumeva sortirane kamencice.
;;; member ne radi ako umesto konkretnih bojeva stavimo parametre!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
(defun polje-usg (kameni smer)
  (cond ((member smer '(4 3 2)) (car kameni))
        ((member smer '(1 5 6)) (car (reverse kameni)))
        (t '())))

;;; USG = u smeru guranja
;;; vraca listu od tri elementa
;;; vraca u obliku ( "x" "o" "-")
(defun nadji-susedna-n-usg (kamen smer tabla n)
  (cond ((= 0 n) '())
        (t (let* ((noviKamen (pomeri-kamen kamen smer)))
             (cons (znak noviKamen tabla) (nadji-susedna-n-usg noviKamen smer tabla (- n 1)))))))

;;; usg = u smeru guranja
;;; vraca listu od tri elementa
;;; vraca cvorove
(defun nadji-susedna-n-usg-koord (kamen smer tabla n)
  (cond ((= 0 n) '())
        (t (let* ((noviKamen (pomeri-kamen kamen smer)))
             (cons (list noviKamen (znak noviKamen tabla)) (nadji-susedna-n-usg-koord noviKamen smer tabla (- n 1)))))))

;;; KAMENI koji igraju potez, SMER u kome se krecu, TABLA na kojoj se nalaze.
(defun moguce-guranje-p (kameni smer tabla)
  (let* ((prviKamen (polje-usg kameni smer))
         (brojKamena (length kameni))
         (susedi (nadji-susedna-n-usg prviKamen smer tabla brojKamena))
         (suprotanZnak (if (equal (znak (car kameni) tabla) "x") "o" "x"))
         (a (format t "prviKamen: ~s~%brojKamena: ~s~%susedi: ~s~%suprotanZnak: ~s~%" prviKamen brojKamena susedi suprotanZnak))
        )
    (cond ((and (equal (car susedi) suprotanZnak)
                (member (cadr susedi) '("-" '() NIL))) 't)
          ((and (= 3 brojKamena)
                (equal (car susedi) suprotanZnak)
                (member (cadr susedi) '("-" '() NIL))) 't)
          ((and (= 3 brojKamena)
                (equal (car susedi) suprotanZnak)
                (equal (cadr susedi) suprotanZnak)
                (member (caddr susedi) '("-" '() NIL))) 't)
          (t '()))))

;;; Proverava da li se KAMENIMA moze odigrati potez.
(defun legalan-unos-p (kameni tabla)
  (and (natabli-p kameni (velicina-table tabla))
       (istogznaka-p (pozicije-u-cvorove kameni tabla))
       (susedni-p kameni)
       (linija-p kameni)
       (<= (length kameni) 3)))

;;; Transformise (0 4 -4) u ((0 4 -4) "x").
(defun pozicije-u-cvorove (kameni tabla)
  (mapcar (lambda (x) (append (list x) (list (znak x tabla)))) kameni))

;;; Trasformise ((0 4 -4) "x") u (0 4 -4).
(defun cvorovi-u-pozicije (cvorovi) (mapcar 'car cvorovi))

;;; Vraca T ili NIL.
(defun legalan-potez (kameni smer tabla)
  (cond ((not (legalan-unos-p kameni tabla)) '())
        (t (let* ((novaPoz (nova-pozicija kameni smer))
                  (novaPoz (sortiraj novaPoz 'op-poredjenja-koord-samo))
                  (smerGuranja (smer-guranja-p kameni smer))
                  (mojZnak (znak (car kameni) tabla))
                  (a (format t "novaPoz: ~s~%smerGuranja: ~s~%mojZnak: ~s~%" novaPoz smerGuranja mojZnak))
                 )
             (cond ((not (natabli-p novaPoz (velicina-table tabla))) '()) ;nova pozicija je van table
                   ((and (not smerGuranja) (equal "-" (sta-se-nalazi (pozicije-u-cvorove novaPoz tabla)))) t) ;ukoliko nije izguravanje mora da se pomeri na prazno polje
                   ((and smerGuranja (equal "-" (znak (polje-usg novaPoz smer) tabla) )) t) ;mozes da se pomeris na prazno polje u smeru guranja
                   ((and smerGuranja (equal mojZnak (znak (polje-usg novaPoz smer) tabla)) ) '()) ;ne mozes da se pomeris ako si ti tamo
                   ((and smerGuranja (moguce-guranje-p kameni smer tabla)) 't) ;ako je moguce guranje moguc je i potez
                   (t '()))))))

;;; Sluzi za paralelno pomeranje.
;;; Kameni se pomeraju na prazno polje.
(defun potez-zamena (staraPozicija novaPozicija tabla mojZnak)
  (postavi-vrednosti staraPozicija "-" (postavi-vrednosti novaPozicija mojZnak tabla)))

;;; Ne gura, samo se pomeri za jedan.
(defun potez-pomeraj-usg (staraPozicija novaPozicija tabla mojZnak)
  (postavi-vrednosti novaPozicija mojZnak (postavi-vrednosti staraPozicija "-" tabla)))

(defun potez-guranje (staraPozicija novaPozicija tabla smer mojZnak)
  (let* ((prviKamen (polje-usg staraPozicija smer))
         (brojKamena (length staraPozicija))
         (susedi (nadji-susedna-n-usg-koord prviKamen smer tabla brojKamena))
         (susedi (sredi-susede-pom susedi))
         (susedi (cvorovi-u-pozicije susedi))
         (a (FORMAT T "~s~%" tabla))
         (tabla (potez-pomeraj-usg susedi (nova-pozicija susedi smer) tabla (kontra-znak mojZnak)))
         (a (FORMAT T "~s~%" tabla))
         (tabla (potez-pomeraj-usg staraPozicija novaPozicija tabla mojZnak))
         (a (FORMAT T "~s~%" tabla))
)
    tabla))

;;; Ostavlja samo listu SUSEDA koji su nekog znaka, eleminise sve '-' i polja van table.
(defun sredi-susede-pom (susedi)
  (cond ((null susedi) '())
        ((not (clanp (cadar susedi) '( "-" '())   )) (cons (car susedi) (sredi-susede-pom (cdr susedi))))
        (t '())))

(defun kontra-znak (znak) (if (equal znak "x") "o" "x"))

(defun odigraj-potez (kameni smer tabla)
   (cond ((not (legalan-unos-p kameni tabla)) '())
         (t (let* ((novaPoz (nova-pozicija kameni smer))
                   (novaPoz (sortiraj novaPoz 'op-poredjenja-koord-samo))
                   (kameni (sortiraj kameni 'op-poredjenja-koord-samo))
                   (smerGuranja (smer-guranja-p kameni smer))
                   (mojZnak (znak (car kameni) tabla))
                   (b (format t "novaPoz: ~s~%smerGuranja: ~s~%mojZnak: ~s~%" novaPoz smerGuranja mojZnak))
                   (c (format t "f-ja:~s~%equal:~s~%"  (sta-se-nalazi (pozicije-u-cvorove novaPoz tabla)) (equalp "-" (sta-se-nalazi (pozicije-u-cvorove novaPoz tabla)))))
                  )
              (cond ((not (natabli-p novaPoz (velicina-table tabla))) '()) ;nova pozicija je van table
                    ((and (not smerGuranja) (equalp "-" (sta-se-nalazi (pozicije-u-cvorove novaPoz tabla)))) (potez-zamena kameni novaPoz tabla mojZnak)) ;ukoliko nije izguravanje mora da se pomeri na prazno polje
                    ((and smerGuranja (equalp "-" (znak (polje-usg novaPoz smer) tabla) )) (potez-pomeraj-usg kameni novaPoz tabla mojZnak)) ;mozes da se pomeris na prazno polje u smeru guranja
                    ((and smerGuranja (equalp mojZnak (znak (polje-usg novaPoz smer) tabla)) ) '()) ;ne mozes da se pomeris ako si ti tamo
                    ((and smerGuranja (moguce-guranje-p kameni smer tabla)) (potez-guranje kameni novaPoz tabla smer mojZnak)) ;ako je moguce guranje moguc je i potez
                    ;( t (format t "dovde"))
                    ( t '()))))))









(defparameter *tabla*
  (kreiraj-tablu 5))


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
                              (if (null (odigraj-potez (car d) (cadr d) *tabla*))
                                  *tabla*
                                  (odigraj-potez (car d) (cadr d) *tabla*)
                                  )
                            )
                        )
      )
    ;;(concatenate 'string "echo: " (eval d))
    )
  )

(defun-ajax reset (data) (*ajax-processor* :callback-data :response-text)
  (format nil "~S~%" (stampaj (setq *tabla* (kreiraj-tablu 5))))
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
