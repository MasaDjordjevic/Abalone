# Uputstvo za upotrebu Masinog otkirićca:

1. Quicklisp
	- file, load, quicklisp.lisp
	- prikazace se komanda za instalaciju
	// prilikom sledece upotrebe se ne loaduje taj fajl nego negi sa c:\nesto\nesto, pisace

2. Instalacija potrebnih paketa
	- samo pokreni sledecu liniju
	- (ql:quickload '(:hunchentoot :cl-who :parenscript :smackjack))
	- kad izbaci gresku izabrati: skip loading C:\Users\Masa\AppData\Local\cache\common-lisp\acl-10.0-win-x86\C\Users\Masa\quicklisp\dists\quicklisp\software\cl+ssl-20160318-git\src\reload.fasl     [EXCL::SKIP]
	
3. Load mog remek dela
	- file, compile and load, izabrati moje remek delo (tutorijal.cl)

4. Sada je server postavljen i radi
	- mozete iz brauzera otvoriti stranice http://localhost:8080/repl
	- kao i /repla i /masa (mada meni masa nece)
	- ako neka nece, klik negde u f-ji, pa ctrl-d, mada nije bitno, ovo necemo koristiti, samo je test
	- u editboxu ne kucati inicijalne zagrade, primer: + 1 2

4. Omoguciti remote ajax call
	- instalirati chorme plugin
	- https://chrome.google.com/webstore/detail/allow-control-allow-origi/nlfbmbojpeacfghkpbjhddihlkkiljbi?hl=en

5. Sad mozemo da pozovemo lisp f-je iz nase stranice
	- test primer je jelte test.html 


Tutorijal koji sam pratila i moze da vam razjasni delove koda: https://blog.jeaye.com/2015/09/27/parenscript-ajax/





