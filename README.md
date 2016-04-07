# Uputstvo za upotrebu Mašinog otkirića

1. **Omogućiti _Remote AJAX call_** (samo jednom)
   - Instalirati ekstenziju za Chorme sa adrese `https://chrome.google.com/webstore/detail/allow-control-allow-origi/nlfbmbojpeacfghkpbjhddihlkkiljbi?hl=en`.

2. **Quicklisp** (svaki put)
   - File > Load > `quicklisp.lisp`
   - Prikazaće se komanda za instalaciju. Prilikom sledeće upotrebe se ne loaduje taj fajl nego sa već isntalirane lokacije (dibager će ponuditi opciju za to).

3. **Instalacija potrebnih paketa** (svaki put)
    - Pokrenuti `(ql:quickload '(:hunchentoot :cl-who :parenscript :smackjack))`
    - Ako se javi greska, izabrati: _skip loading_
 
4. **Load našeg remek dela** 
    - File > Compile and load > `sve.cl`

5. **Otvoriti `index.html`**