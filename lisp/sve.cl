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


;;; ultimate brisanje nilova
(defun obrisi-nil (lista)
  (cond ((null lista) '())
        ((and (listp (car lista)) (=(length (car lista)))) (obrisi-nil (cdr lista)))
        (t (cons (car lista) (obrisi-nil (cdr lista))))))

;;; Test na ciljno stanje.
;;; Ocekuje cvorove.
(defun kraj-p (tabla)
  (or (<= (prebroji "x" tabla) 8) (<= (prebroji "o" tabla) 8)))

;;; Pomocna funkcija.
(defun prebroji (znak tabla)
  (cond ((null tabla) 0)
        ((equal (cadar tabla) znak) (1+ (prebroji znak (cdr tabla))))
        (t (prebroji znak (cdr tabla)))))


;;; Vrati suprotan znak.
(defun suprotan-znak (znak) (if (equal znak "x") "o" "x"))




















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
(defun istog-znaka-p (kameni)
  (cond ((null (cdr kameni)) 't)
        ((istogznaka2-p (car kameni) (cadr kameni)) (istog-znaka-p (cdr kameni)))
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
  (cond ((not (istog-znaka-p cvorovi)) "m")
        (t (cadar cvorovi)))) ; ako je stigao dovde, znaci da su svi istog znaka,
                              ; pa vraca znak prvog cvora

;;; Vraca onaj kamen "koji gura".
;;; Podrazumeva sortirane kamencice.
;;; member ne radi ako umesto konkretnih bojeva stavimo parametre!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
(defun polje-usg (kameni smer)
  (let* ((kameni (sortiraj kameni 'op-poredjenja-koord-samo)))
    (cond ((member smer '(4 3 2)) (car kameni))
          ((member smer '(1 5 6)) (car (reverse kameni)))
          (t '()))))

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
    (cond ((= 1 brojKamena) '())
          ((and (equal (car susedi) suprotanZnak)
                (member (cadr susedi) '("-" '() NIL) :test 'equalp)) 't)
          ((and (= 3 brojKamena)
                (equal (car susedi) suprotanZnak)
                (member (cadr susedi) '("-" '() NIL) :test 'equalp)) 't)
          ((and (= 3 brojKamena)
                (equal (car susedi) suprotanZnak)
                (equal (cadr susedi) suprotanZnak)
                (member (caddr susedi) '("-" '() NIL) :test 'equalp)) 't)
          (t '()))))

;;; Proverava da li se KAMENIMA moze odigrati potez.
(defun legalan-unos-p (kameni tabla)
  (and (natabli-p kameni (velicina-table tabla))
       (istog-znaka-p (pozicije-u-cvorove kameni tabla))
       (susedni-p kameni)
       (linija-p kameni)
       (<= (length kameni) 3)))

;;; Transformise (0 4 -4) u ((0 4 -4) "x").
(defun pozicije-u-cvorove (kameni tabla)
  (mapcar (lambda (x) (list x (znak x tabla))) kameni))

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
         ;(a (FORMAT T "~s~%" tabla))
         (tabla (potez-pomeraj-usg susedi (nova-pozicija susedi smer) tabla (suprotan-znak mojZnak)))
         ;(a (FORMAT T "~s~%" tabla))
         (tabla (potez-pomeraj-usg staraPozicija novaPozicija tabla mojZnak))
         ;(a (FORMAT T "~s~%" tabla))
)
    tabla))

;;; Ostavlja samo listu SUSEDA koji su nekog znaka, eleminise sve '-' i polja van table.
(defun sredi-susede-pom (susedi)
  (cond ((null susedi) '())
        ((not (clanp (cadar susedi) '( "-" '()))) (cons (car susedi) (sredi-susede-pom (cdr susedi))))
        (t '())))

(defun odigraj-potez (kameni smer tabla)
   (cond ((not (legalan-unos-p (sortiraj kameni 'op-poredjenja-koord-samo) tabla)) '())
         (t (let* ((kameni (sortiraj kameni 'op-poredjenja-koord-samo))
                   (novaPoz (nova-pozicija kameni smer))
                   (novaPoz (sortiraj novaPoz 'op-poredjenja-koord-samo))
                   (smerGuranja (smer-guranja-p kameni smer))
                   (mojZnak (znak (car kameni) tabla))
                   ;(b (format t "novaPoz: ~s~%smerGuranja: ~s~%mojZnak: ~s~%(polje-usg novaPoz smer): ~s~%" novaPoz smerGuranja mojZnak (polje-usg novaPoz smer)))
                   ;(c (format t "sta-se-nalazi: ~s~%"  (sta-se-nalazi (pozicije-u-cvorove novaPoz tabla))))
                  )
              (cond ((not (natabli-p novaPoz (velicina-table tabla))) '()) ;nova pozicija je van table
                    ((and (not smerGuranja) (equalp "-" (sta-se-nalazi (pozicije-u-cvorove novaPoz tabla)))) (potez-zamena kameni novaPoz tabla mojZnak)) ;ukoliko nije izguravanje mora da se pomeri na prazno polje
                    ((and smerGuranja (equalp "-" (znak (polje-usg novaPoz smer) tabla))) (potez-pomeraj-usg kameni novaPoz tabla mojZnak)) ;mozes da se pomeris na prazno polje u smeru guranja
                    ((and smerGuranja (equalp mojZnak (znak (polje-usg novaPoz smer) tabla))) '()) ;ne mozes da se pomeris ako si ti tamo
                    ((and smerGuranja (moguce-guranje-p kameni smer tabla)) (potez-guranje kameni novaPoz tabla smer mojZnak)) ;ako je moguce guranje moguc je i potez
                    ;( t (format t "dovde"))
                    ( t '()))))))



;;; lista moze da bude i tabla
(defun izdvoji-sve-istog-znaka (lista znak)
  (cond ((null lista) '())
        ((equal (cadar lista) znak) (cons (car lista) (izdvoji-sve-istog-znaka (cdr lista) znak)))
        (t (izdvoji-sve-istog-znaka (cdr lista) znak))))

(defun susedi-istog-znaka (tabla cvor)
  (let* ((susedi (kreiraj-susede (car cvor)))
         (susedi (pozicije-u-cvorove susedi tabla)))
    (izdvoji-sve-istog-znaka susedi (cadr cvor))))

(defun jedan-sa-svakim (el lista)
  (cond ((null lista) '())
        (t (append (list (list el (car lista))) (jedan-sa-svakim el (cdr lista))))))

(defun jedan-sa-susedima (cvor tabla)
  (let* ((susedi (susedi-istog-znaka tabla cvor))
         (resenje (jedan-sa-svakim cvor susedi)))
    (mapcar (lambda (x) (sortiraj x 'op-poredjenja)) resenje)))

(defun jedni-sa-susedima (lista tabla)
  (cond ((null lista) '())
        (t (dopuni-listu (jedan-sa-susedima (caar lista) tabla) (jedni-sa-susedima (cdr lista) tabla)))))

;;; ocekuje listu ciji su elementi liste od po dva susedna cvora istog znaka
(defun dodaj-sve-trece (lista tabla)
  (cond ((null lista) '())
        ((istog-znaka-p (car (dodaj-trece (car lista) tabla))) (dopuni-listu (list (car (dodaj-trece (car lista) tabla))) (dodaj-sve-trece (cdr lista) tabla)))
        ((istog-znaka-p (cadr (dodaj-trece (car lista) tabla))) (dopuni-listu (cdr (dodaj-trece (car lista) tabla)) (dodaj-sve-trece (cdr lista) tabla)))
        (t (dodaj-sve-trece (cdr lista) tabla))))

;;; lista je lista od dva susedna cvora istog znaka
(defun dodaj-trece (lista tabla)
  (let* ((a (caar lista))
         (b (caadr lista))
         (resenje1 (loop for p in a
                         for q in b
                       collect (+ p (- p q))))
         (resenje2 (loop for q in a
                         for p in b
                       collect  (+ p (- p q))))
         (resenje1 (pozicije-u-cvorove (list resenje1) tabla))
         (resenje2 (pozicije-u-cvorove (list resenje2) tabla))
         (resenje (list (append lista resenje1) (append lista resenje2))))
    (mapcar (lambda (x) (sortiraj x 'op-poredjenja)) resenje)))

;;; ocekuje cvorove
(defun nadji-sve-potezabilne-kamenove (tabla znak)
  (let* ((jedan (izdvoji-sve-istog-znaka tabla znak))
         (jedan (mapcar (lambda (x) (list x)) jedan))
         ;(a (format t "Jedan: ~s~%" jedan))
         (dva (jedni-sa-susedima jedan tabla))
         ;(a (format t "Dva: ~s~%" dva))
         (tri (dodaj-sve-trece dva tabla))
         ;(a (format t "Tri: ~s~%" tri))
         )
    (append jedan dva tri)))

(defun nova-stanja (tabla znak)
  (let* ((potezabilni (nadji-sve-potezabilne-kamenove tabla znak))
         (potezabilni (mapcar (lambda (x) (cvorovi-u-pozicije x)) potezabilni))
         ;(potezabilni (last potezabilni '6))
         ;(a (format t "Potezabilni: ~s~%" potezabilni))
         (stanja (loop for smer in '(1 2 3 4 5 6)
                     append (mapcar (lambda (x) (odigraj-potez x smer tabla)) potezabilni)))
         (stanja (remove NIL stanja))
         ;(a (format t "stanja: ~s~%" stanja))
         )
    ;(dolist (stanje stanja)
    ;  (print (stampaj stanje)))))
    stanja))






(defun string-u-tabla (string)
     (string-u-tabla-1 string (mapcar 'car (kreiraj-tablu 5))))

(defun string-u-tabla-1 (string tabla)
  (cond ((null tabla) '())
        (t (cons (list (car tabla) (format nil "~a" (char string 0)))
                 (string-u-tabla-1 (subseq string 1 (length string)) (cdr tabla))))))

;;; Heuristika.
;;; X je MAX igrac.

;;; Ocekuje koordinate.
(defun rastojanje (kamen1 kamen2)
  (max (abs (- (car kamen1) (car kamen2)))
       (abs (- (cadr kamen1) (cadr kamen2)))
       (abs (- (caddr kamen1) (caddr kamen2)))))

;;; Ocekuje koordinate.
(defun rastojanje-centar (kamen)
  (rastojanje kamen '(0 0 0)))

;;; Ocekuje koordinate.
(defun rastojanje-do-svih (kamen lista)
  (cond ((null lista) '0)
        (t (+ (rastojanje kamen (car lista)) (rastojanje-do-svih kamen (cdr lista))))))

;;; Ocekuje koordinate.
(defun prosecno-rastojanje-do-svih (kamen lista)
  (cond ((= (length lista) 0) 0)
        ((= (length lista) 1) (rastojanje kamen (car lista)))
        (t (/ (rastojanje-do-svih kamen lista) (if (member kamen lista)
                                                   (1- (length lista))
                                                   (length lista))))))

;;; Ocekuje koordinate.
(defun prosecno-rastojanje-do-centra (lista)
  (cond ((= (length lista) 0) 0)
        ((= (length lista) 1) (rastojanje-centar (car lista)))
        (t (/ (rastojanje-do-svih '(0 0 0) lista) (length lista)))))

;;; Ocekuje cvorove.
(defun broj-izguranih (znak tabla)
  (- 14 (prebroji znak tabla)))

;;; Ocekuje cvorove.
(defun pobeda-p (znak tabla)
  (< (prebroji znak tabla) 8))

;;; Ocekuje cvorove.
(defun h-pobeda (tabla faktor)
  (cond ((pobeda-p "x" tabla) faktor)
        ((pobeda-p "o" tabla) (- 0 faktor))
        (t 0)))

;;; Ocekuje cvorove.
(defun h-izgurani (tabla faktor)
  (* (- (broj-izguranih "o" tabla) (broj-izguranih "x" tabla)) faktor))

;;; Ocekuje cvorove.
(defun h-centar (tabla faktor)
  (* (- (prosecno-rastojanje-do-centra (cvorovi-u-pozicije (izdvoji-sve-istog-znaka tabla "o")))
        (prosecno-rastojanje-do-centra (cvorovi-u-pozicije (izdvoji-sve-istog-znaka tabla "x")))) faktor))

;;; Ocekuje cvorove.
(defun h-grupisanje-1 (lista svi)
  (cond ((null lista) '0)
        (t (+ (prosecno-rastojanje-do-svih (car lista) svi) (h-grupisanje-1 (cdr lista) svi)))))

;;; Ocekuje cvorove.
(defun h-grupisanje (tabla faktor)
  (let* ((svi (cvorovi-u-pozicije (izdvoji-sve-istog-znaka tabla "x")))
         (svi-njegovi (cvorovi-u-pozicije (izdvoji-sve-istog-znaka tabla "o"))))
    (* (- (h-grupisanje-1 svi-njegovi svi-njegovi) (h-grupisanje-1 svi svi)) faktor)))


;;; Ocekuje cvorove.
(defun heuristika-parametri (tabla faktor-pobeda faktor-izgurani faktor-centar faktor-grupisanje)
  (+ (h-pobeda     tabla faktor-pobeda)
     (h-izgurani   tabla faktor-izgurani)
     (h-centar     tabla faktor-centar)
     (h-grupisanje tabla faktor-grupisanje)))

(defun deskriptivna-heuristika (tabla faktor-pobeda faktor-izgurani faktor-centar faktor-grupisanje)
  (format t "Pobeda: ~s~%Izgurani: ~s~%Centar: ~s~%Grupisanje: ~s~%Zbir: ~s~%"
    (float (h-pobeda     tabla faktor-pobeda))
    (float (h-izgurani   tabla faktor-izgurani))
    (float (h-centar     tabla faktor-centar))
    (float (h-grupisanje tabla faktor-grupisanje))
    (float (heuristika-parametri tabla faktor-pobeda faktor-izgurani faktor-centar faktor-grupisanje))))


;;; 1. pobeda
;;; 2. izgurani
;;; 3. centar
;;; 4. grupisanje
(defun heuristika (tabla)
  (float (heuristika-parametri tabla 999999999 100 10 10)))






;;; Minimax algoritam
;;; X je MAX, O je MIN

;;; Od prosledjene lista STANJA napravi listu ciji su elementi (stanje heuristika).
(defun dodaj-heuristike (stanja)
  (cond ((null stanja) '())
        (t (cons (list (car stanja) (heuristika (car stanja)))
                 (dodaj-heuristike (cdr stanja))))))

;;; Iz LISTE ciji su elementi (stanje heuristika) izaberi stanje sa najvecom/najmanjom (>/<) heuristikom.
;; Pomocna funkcija
(defun minmax-stanje-i (lista max-stanje znak)
  (cond ((null lista) max-stanje)
        ((apply znak (list (cadar lista) (cadr max-stanje))) (minmax-stanje-i (cdr lista) (car lista) znak)) ; nadjen novi maksimum, on se prosledjuje dalje
        (t (minmax-stanje-i (cdr lista) max-stanje znak)))) ; trenutni maksimum se prosledjuje dalje
;; Glavna funkcija (polazi od pretpostavke da je prvi element najveci/najmanji.
(defun minmax-stanje (lista znak)
  (minmax-stanje-i (cdr lista) (car lista) znak))

;;; Konkretne funkcije za min i max
(defun min-stanje (lista) (minmax-stanje lista '<))
(defun max-stanje (lista) (minmax-stanje lista '>))

;;; Glavna funkcija za minimax algoritam.
;;; Proceni stanje na osnovu procene potomaka (tj. poteza u koje moze da se stigne iz STANJE) i igraca koji je na potezu.
;;; "x" = max, "o" = min.
;;; Pretraga se okocava kada se dosegne zadatak DUBINA, i tada se kao procena stanja vraca njegova heuristika.
;;; Podrazumevano se kao funkcija koja vraca nova stanja zove "nove-stanja", a za procenu stanja se zove "heuristika", 
;;;   ali se moze promeniti proslednjivanjem opcionih parametara.
(defun minimax (stanje dubina igrac &optional (fja-nova-stanja 'nova-stanja) (fja-proceni-stanje 'heuristika))
  (let* ((igrac-sad      (if (equalp igrac "x") 'max-stanje 'min-stanje))
         (lista-potomaka (apply fja-nova-stanja (list stanje igrac))))
    (cond ((or (zerop dubina) (null lista-potomaka)) (list stanje (apply fja-proceni-stanje (list stanje))))
          (t (apply igrac-sad (list (mapcar (lambda (x) (minimax x (1- dubina) (suprotan-znak igrac) fja-nova-stanja fja-proceni-stanje)) lista-potomaka)))))))


;;; Alfa-beta odsecanje
;; Mora jednako da bismo vratili levi cvor (nebitno za alfa/beta, ngo zbog vracanja cvora)
(defun manje-stanje (stanje1 stanje2)
  (if (<= (cadr stanje1) (cadr stanje2))
      stanje1 stanje2))
(defun vece-stanje (stanje1 stanje2)
  (if (>= (cadr stanje1) (cadr stanje2))
      stanje1 stanje2))


(defparameter *ALPHA* '('() -999999999))
(defparameter *BETA*  '('() +999999999))
(defun alpha-beta-max (stanje alpha beta dubina maxdubina &optional (fja-nova-stanja 'nova-stanja) (fja-proceni-stanje 'heuristika))
  (if (zerop dubina)
      (list stanje (apply fja-proceni-stanje (list stanje)))
    (loop for sledbenik in (apply fja-nova-stanja (list stanje "x"))
        do (if (>= (cadr (setq alpha (vece-stanje alpha
                                                  (alpha-beta-min sledbenik alpha beta (1- dubina) maxdubina fja-nova-stanja fja-proceni-stanje))))
                   (cadr beta))
               ;; vraca sebe:
               ;(return (list stanje (cadr beta))))
               ;finally (return (list stanje (cadr alpha))))))
               ;; vraca najdublji dokle je stigo pa je skontao da je najbolje resenje:   
               ;(return beta))
               ;finally (return alpha))))
               ;; vraca sledeci potez koji treba da se odigra
               (return (if (= dubina maxdubina) beta (list stanje (cadr beta)))))
        finally (return (if (= dubina maxdubina) alpha (list stanje (cadr alpha)))))))
               

(defun alpha-beta-min (stanje alpha beta dubina maxdubina &optional (fja-nova-stanja 'nova-stanja) (fja-proceni-stanje 'heuristika))
  (if (zerop dubina)
      (list stanje (apply fja-proceni-stanje (list stanje)))
    (loop for sledbenik in (apply fja-nova-stanja (list stanje "o"))
        do (if (<= (cadr (setq beta (manje-stanje beta
                                                  (alpha-beta-max sledbenik alpha beta (1- dubina) maxdubina fja-nova-stanja fja-proceni-stanje))))
                   (cadr alpha))
               ;; vraca sebe:
               ;(return (list stanje (cadr alpha))))
               ;finally (return (list stanje (cadr beta))))))
               ;; vraca najdublji dokle je stigo pa je skontao da je najbolje resenje:   
               ;(return alpha))
               ;finally (return beta))))
               ;; vraca sledeci potez koji terba da se odgira
               (return (if (= dubina maxdubina) alpha (list stanje (cadr alpha)))))
        finally (return (if (= dubina maxdubina) beta (list stanje (cadr beta)))))))
               
               

;;; Primeri za testiranje.
;;; (MINIMAX 'A '4 "x" '--SAJT-NOVA-STANJA '--SAJT-PROCENI-STANJE)
;;; (ALPHA-BETA-MAX 'A *ALPHA* *BETA* '4 '4 '--SAJT-NOVA-STANJA '--SAJT-PROCENI-STANJE)
;; Sa slajdova (strane 46/4 i 47/1).
;; A -> 3
(defun --slajdovi-nova-stanja (stanje &optional (znak "x")) 
  (case stanje
    ((a) '(b c d))    ((b) '(e f))     ((c) '(g h))    ((d) '(i j))
    ((e) '(k l))      ((f) '(m n))     ((g) '(o))      ((h) '(p q))
    ((i) '(r s))      ((j) '(t u))     (t '())))
(defun --slajdovi-proceni-stanje (stanje)   
  (case stanje
    ((k) 2)    ((l) 3)    ((m) 5)    ((n) 9)    ((o) 0)    ((p) 7)
    ((q) 4)    ((r) 2)    ((s) 1)    ((t) 5)    ((u) 6)    (t 0)))

;; Sa sajta http://web.cs.ucla.edu/~rosen/161/notes/alphabeta.html
;; A -> 3
(defun --sajt-nova-stanja (stanje &optional (znak "x"))  
  (case stanje
    ((a) '(b c))    ((b) '(d e))    ((c) '(f g))    ((d) '(h i))
    ((e) '(j k))    ((f) '(l m))    ((g) '(n))      ((h) '(o p))
    ((i) '(q r))    ((j) '(s))      ((k) '(t u))    ((l) '(v w))
    ((m) '(x))      ((n) '(y z))    (t '())))
(defun --sajt-proceni-stanje (stanje)   
  (case stanje
    ((o) 3)    ((p) 17)    ((q) 2)    ((r) 12)    ((s) 15)    ((t) 25)
    ((u) 0)    ((v) 2)     ((w) 5)    ((x) 3)     ((y) 2)     ((z) 14)
    (t 0)))

(defun vreme ()
  (let ((real1 (get-internal-real-time))
        (run1 (get-internal-run-time)))
    ;(ALPHA-BETA-MAX *TABLA* *ALPHA* *BETA* '4 '4)
    (ALPHA-BETA-MAX *TABLA* *ALPHA* *BETA* '3 '3) 
    ;(MINIMAX *TABLA* 2 "x")
    (let ((run2 (get-internal-run-time))
	    (real2 (get-internal-real-time)))
	(format t "Computation took:~%")
	(format t "  ~f seconds of real time~%"
		(/ (- real2 real1) internal-time-units-per-second))
	(format t "  ~f seconds of run time~%"
		(/ (- run2 run1) internal-time-units-per-second)))))


















(defun stampa1 (i n lp)
  (cond ((null lp) '())
        ((equal i n) (progn (format t "~%") (stampa1 0 n lp)))
        (t (progn (format t "~a " (car lp)) (stampa1 (1+ i) n (cdr lp))))))


(defun stampa (l)
  (stampa1 0 6 l))


(defparameter *tabla*
  (kreiraj-tablu 5))


; Allow cl-who and parenscript to work together
(setf *js-string-delimiter* #\")

(defparameter *ajax-processor*
  (make-instance 'ajax-processor :server-uri "/repl-api"))

(defun heuristika-ajax-1 (tabla faktor-pobeda faktor-izgurani faktor-centar faktor-grupisanje)
  (let* ((tabla (string-u-tabla tabla)))
    (format t "Pobeda: ~s~%Izgurani: ~s~%Centar: ~s~%Grupisanje: ~s~%Zbir: ~s~%"
      (float (h-pobeda     tabla faktor-pobeda))
      (float (h-izgurani   tabla faktor-izgurani))
      (float (h-centar     tabla faktor-centar))
      (float (h-grupisanje tabla faktor-grupisanje))
      (float (heuristika-parametri tabla faktor-pobeda faktor-izgurani faktor-centar faktor-grupisanje)))))

(defun-ajax heuristikaAJAX (data) (*ajax-processor* :callback-data :response-text)
  (let* (;(znak data) ; zapravo je nebitan parametar
         (tabla *tabla*)
         (faktor-pobeda 1)
         (faktor-izgurani 1)
         (faktor-centar 1)
         (faktor-grupisanje 1))
    (format nil "Pobeda: ~s~%Izgurani: ~s~%Centar: ~s~%Grupisanje: ~s~%Zbir: ~s~%"
      (float (h-pobeda     tabla faktor-pobeda))
      (float (h-izgurani   tabla faktor-izgurani))
      (float (h-centar     tabla faktor-centar))
      (float (h-grupisanje tabla faktor-grupisanje))
      (float (heuristika-parametri tabla faktor-pobeda faktor-izgurani faktor-centar faktor-grupisanje)))))

(defun-ajax echo (data) (*ajax-processor* :callback-data :response-text)
  (let ((d (string-to-list data)))
    (format nil "~S~%" (stampaj (setq *tabla* (if (null (odigraj-potez (car d) (cadr d) *tabla*))
                                                  *tabla*
                                                (odigraj-potez (car d) (cadr d) *tabla*)))))))

;;; Neka AI razmisli
(defun ai-odigraj-potez-1 (tabla znak)
  (car (minimax tabla (if (equalp znak "x") 60 3) znak)))
(defun-ajax ai-odigraj-potez (data) (*ajax-processor* :callback-data :response-text)
  (format nil "~S~%" (stampaj (setq *tabla* (ai-odigraj-potez-1 (string-u-tabla (subseq data 1 (length data)))
                                                                (subseq data 0 1))))))


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






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                                              ;;;
;;;                                          TEST UNITI                                          ;;;
;;;                                                                                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 8. poglavlje

(defmacro do-primes ((var start end) &body body)
  (let ((ending-value-name (gensym)))
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
          (,ending-value-name ,end))
         ((> ,var ,ending-value-name))
       ,@body)))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))


;;; 9. poglavlje

(defvar *test-name* nil)

(defmacro deftest (name parameters &body body)
  "Define a test function. Within a test function we can call
   other test functions or use 'check' to run individual test
   cases."
  `(defun ,name ,parameters
    (let ((*test-name* (append *test-name* (list ',name))))
      ,@body)))

(defmacro check (&body forms)
  `(combine-results
    ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro combine-results (&body forms)
  (with-gensyms (result)
    `(let ((,result t))
      ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
       ,result)))

(defun report-result (result form)
  "Report the results of a single test case. Called by 'check'."
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                           
  
  



