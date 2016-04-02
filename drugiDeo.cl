;(in-package :jank-repl)

(defun smer (n)
  (cond ((= n 4) '(-1 1 0))   ; levo
        ((= n 3) '(0 1 -1))   ; gore-levo
        ((= n 2) '(1 0 -1))   ; gore-desno
        ((= n 1) '(1 -1 0))   ; desno
        ((= n 5) '(0 -1 1))   ; dole-desno
        ((= n 5) '(-1 0 1)))) ; dole-levo

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
    (or (and (= linija 0) (or (= smer 2) (= smer 5)))    ; x
        (and (= linija 1) (or (= smer 3) (= smer 6)))    ; y
        (and (= linija 2) (or (= smer 1) (= smer 4)))))) ; z

;;; Vraca jedan karakter koji oznacava da li se na tom skupu CVOROVA nalazi:
;;; samo x, samo o, samo -, ili nesto mesano 'm'
;;; pozicija je niz koorindata
(defun sta-se-nalazi (cvorovi)
  (cond ((= 1 (length cvorovi)) "m")
        ((not (istogznaka-p cvorovi)) "m")
        (t (cadar cvorovi)))) ; ako je stigao dovde, znaci da su svi istog znaka,
                              ; pa vraca znak prvog cvora

;;; Vraca onaj kamen "koji gura".
;;; Podrazumeva sortirane kamencice.
(defun polje-usg (kameni smer)
  (cond ((member smer '(1 2 3)) (car kameni))
        ((member smer '(4 5 6)) (car (reverse kameni)))
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
         ;(a (format t "prviKamen: ~s~%brojKamena: ~s~%susedi: ~s~%suprotanZnak: ~s~%" prviKamen brojKamena susedi suprotanZnak))
        )
    (cond ((and (equal (car susedi) suprotanZnak)
                (member (cadr susedi) '("-" '()))) 't)
          ((and (= 3 brojKamena)
                (equal (car susedi) suprotanZnak)
                (equal (cadr susedi) suprotanZnak)
                (member (caddr susedi) '("-" '()))) 't)
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
(defun legalanPotez (kameni smer tabla)
  (cond ((not (legalan-unos-p kameni tabla)) '())
        (t (let* ((novaPoz (nova-pozicija kameni smer))
                  (novaPoz (sortiraj novaPoz 'opPoredjenjaKoordSamo))
                  (smerGuranja (smer-guranja-p kameni smer))
                  (mojZnak (znak (car kameni) tabla))
                  ;(a (format t "novaPoz: ~s~%smerGuranja: ~s~%mojZnak: ~s~%" novaPoz smerGuranja mojZnak))
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
  (postaviVrednosti staraPozicija "-" (postaviVrednosti novaPozicija mojZnak tabla)))

;;; Ne gura, samo se pomeri za jedan.
(defun potez-pomeraj-usg (staraPozicija novaPozicija tabla mojZnak)
  (postaviVrednosti novaPozicija mojZnak (postaviVrednosti staraPozicija "-" tabla)))

(defun potez-guranje (staraPozicija novaPozicija tabla smer mojZnak)
  (let* ((prviKamen (polje-usg staraPozicija smer))
         (brojKamena (length staraPozicija))
         (susedi (nadji-susedna-n-usg-koord prviKamen smer tabla brojKamena))
         (susedi (sredi-susede-pom susedi))
         (susedi (cvorovi-u-pozicije susedi))
         (tabla (potez-pomeraj-usg susedi (nova-pozicija susedi smer) tabla (kontra-znak mojZnak)))
         (tabla (potez-pomeraj-usg staraPozicija novaPozicija tabla mojZnak)))
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
                   (novaPoz (sortiraj novaPoz 'opPoredjenjaKoordSamo))
                   (kameni (sortiraj kameni 'opPoredjenjaKoordSamo))
                   (smerGuranja (smer-guranja-p kameni smer))
                   (mojZnak (znak (car kameni) tabla))
                   ;(b (format t "novaPoz: ~s~%smerGuranja: ~s~%mojZnak: ~s~%" novaPoz smerGuranja mojZnak))
                  )
              (cond ((not (natabli-p novaPoz (velicina-table tabla))) '()) ;nova pozicija je van table
                    ((and (not smerGuranja) (equal "-" (sta-se-nalazi (pozicije-u-cvorove novaPoz tabla)))) (potez-zamena kameni novaPoz tabla mojZnak)) ;ukoliko nije izguravanje mora da se pomeri na prazno polje
                    ((and smerGuranja (equal "-" (znak (polje-usg novaPoz smer) tabla) )) (potez-pomeraj-usg kameni novaPoz tabla mojZnak)) ;mozes da se pomeris na prazno polje u smeru guranja
                    ((and smerGuranja (equal mojZnak (znak (polje-usg novaPoz smer) tabla)) ) '()) ;ne mozes da se pomeris ako si ti tamo
                    ((and smerGuranja (moguce-guranje-p kameni smer tabla)) (potez-guranje kameni novaPoz tabla smer mojZnak)) ;ako je moguce guranje moguc je i potez
                    ;( t (format t "dovde"))
                    ( t '()))))))
