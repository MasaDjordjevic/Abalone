var isMouseDown = false;

class Koordinata {
    constructor(public x: number = 0, public y: number = 0, public z: number = 0) { }
}

class Kamen {
    div: HTMLElement;

    constructor(
        public tabla: Tabla,
        public koordinata: Koordinata = new Koordinata(),
        public boja: string = '-'
        ) { }

    stampaj(size: number = 67): void {
        var kamen = document.createElement('div');

        // koji je znak na kamenu
        kamen.classList.add('polje');
        kamen.classList.add('polje-' + this.boja);

        var x = this.koordinata.x * size;
        var y = this.koordinata.y * size;
        var z = this.koordinata.z * size;

        var left = x + z / 2;
        var top = z;
        top -= this.koordinata.z * (size * 9 / 67);

        // Korekcija da bude u centar
        //top  += (- size / 2 + 5 * size * Math.sqrt(3.0) / 2);
        top += (- size / 2 + 5 * size);
        left += (- size / 2 + 5 * size);

        kamen.style.top = top.toString() + 'px';
        kamen.style.left = left.toString() + 'px';



        // upis u kamen, kubne koordinate
        kamen.innerHTML = '<span class="koordinate cubic">' + this.koordinata.x + ', ' + this.koordinata.y + ', ' + this.koordinata.z + '</span>';

        // upis u kamen, aksijalne kooridnate
        var pbroj = this.koordinata.z;
        pbroj = 2 * this.tabla.velicina - 1 - pbroj;
        var p = String.fromCharCode(93 + (pbroj - 1)).toUpperCase();
        var q = (this.koordinata.x) + 2 * this.tabla.velicina - 5;
        kamen.innerHTML += '<span class="koordinate axial">' + p + q + '</span>';

        this.tabla.div.appendChild(kamen);
        this.div = kamen;

        this.div.onmousedown = (e) => {
            isMouseDown = true;
            this._onclick();
        }

        this.div.onmouseenter = (e) => {
            if (isMouseDown) {
                this._onclick();
            }
        }


    }

    selektiraj(nacin: string = "-dobro") {
        this.div.classList.add('selektiran');
        this.div.classList.add('selektiran' + nacin);
    }


    deselektiraj() {
        this.div.classList.remove('selektiran');
        this.div.classList.remove('selektiran-lose');
        this.div.classList.remove('selektiran-dobro');
    }

    _onclick() {
        //ako je ukljucen striktni rezim i ako ti nisi na redu oboji -lose
        if ((<HTMLInputElement>document.getElementsByName("strict-mode")[0]).checked) {
            var naRedu = this.tabla.naRedu;
            var naReduZnak = this.tabla.naRedu == 0 ? "x" : "o";
            if (naReduZnak == this.boja) {
                this.tabla.toggleKamen(this, "-dobro");
            }
            else {
                this.tabla.toggleKamen(this, "-lose");
            }

        }
        else {
            this.tabla.toggleKamen(this);
        }


    }
}

enum Igrac {
    Human,
    AI
};

class Tabla {
    div: HTMLElement;
    polja: Kamen[];
    selektirani: Kamen[];
    velicina: number;
    poslednjeStanje: string;
    igraci: Igrac[] = new Array<Igrac>(Igrac.Human, Igrac.Human);
    naRedu: number = 0;
    velicinaKamencica : number = 70;
    heuristikaX = [];
    heuristikaO = [];

    constructor(velicina: number, igrac0: Igrac = Igrac.Human, igrac1: Igrac = Igrac.Human, HTMLtabla:HTMLElement, velicinaKamencica: number, miniTabla: boolean = true) {
        this.velicina = velicina;

        this.igraci[0] = igrac0;
        this.igraci[1] = igrac1;

        this.polja = new Array<Kamen>(3 * velicina * velicina - 3 * velicina + 1); // 3n² - 3n + 1
        for (var i = 0; i < this.polja.length; i++) {
            this.polja[i] = new Kamen(this);
        }

        if (miniTabla)
          this.resetNaRedu();
        this.poslednjeStanje = null;

        this.div = HTMLtabla;
        this.velicinaKamencica = velicinaKamencica;

        //postavljanje koordinata kamencicima
        var index = 0;
        for (var z = -(velicina - 1); z <= (velicina - 1); z++) {
            var duzinaReda = 2 * velicina - 1 - Math.abs(z); // duzinaReda + abs(z) = 2*velicina -1
            var x, y;
            if (z <= 0) {
                // gornja polovina, sa polovinom
                y = velicina - 1;
                x = 0 - z - y;
            } else {
                // donja polovina
                x = -(velicina - 1);
                y = 0 - x - z;
            }
            for (var i = 0; i < duzinaReda; i++) {
                this.polja[index].koordinata.x = x + i;
                this.polja[index].koordinata.y = y - i;
                this.polja[index].koordinata.z = z;
                index++;
            }
        }

        this.selektirani = [];

        this.nacrtaj();
    }

    resetNaRedu(): void {
        this.naRedu = 0;
        if (!document.getElementById("stats-x").classList.contains("trenutni-na-redu"))
            document.getElementById("stats-x").classList.add("trenutni-na-redu");
        document.getElementById("stats-o").classList.remove("trenutni-na-redu");
    }

    toggleNaRedu(): void {
        this.naRedu ^= 1;
        document.getElementById("stats-x").classList.toggle("trenutni-na-redu");
        document.getElementById("stats-o").classList.toggle("trenutni-na-redu");
    }

    setPoslednjeStanje(s: string): void {
        this.poslednjeStanje = s.replace(/[^XxOo\-\_]/g, '');
    }

    trenutnoStanje(): string {
        var returnValue: string = "";
        for (let i = 0; i < this.polja.length; i++) {
            returnValue += this.polja[i].boja;
        }
        return returnValue;
    }

    nacrtaj(): void {
        this.selektirani = [];
        var HTMLselektirani = document.getElementById('selektirani');
        if (HTMLselektirani !== null) {
            HTMLselektirani.innerHTML = '';
        }

        this.div.innerHTML = '';

        for (var i = 0; i < this.polja.length; i++) {
            this.polja[i].stampaj(this.velicinaKamencica);
        }

        var width: number = 2 * 5 * this.velicinaKamencica;
        //var height:number = 5 * size * Math.sqrt(3.0);
        this.div.style.width = String(width) + "px";
        this.div.style.height = String(width) + "px";
    }

    toggleKamen(kamen: Kamen, nacin: string = "-dobro"): void {
        var index = this.selektirani.indexOf(kamen);
        if (index == -1) {
            // nema ga
            this.selektirajKamen(kamen, nacin);
        } else {
            // ima ga
            this.deselektirajKamen(kamen);
        }

        // Apdejtovanje DIV-a
        if (document.getElementById('selektirani') !== null) {
            var string = '';
            for (var i = 0; i < this.selektirani.length; i++) {
                string += '<li>';
                string += this.selektirani[i].koordinata.x + ', ' + this.selektirani[i].koordinata.y + ', ' + this.selektirani[i].koordinata.z;
                string += '</li>';
            }
            document.getElementById('selektirani').innerHTML = string;
        }
    }

    selektirajKamen(kamen: Kamen, nacin: string): void {
        // var size = this.selektirani.filter(function(value) {return value !== undefined}).length;
        if (this.selektirani.length >= 3) {
            this.selektirani[0].deselektiraj();
            this.selektirani.splice(0, 1);
        }
        this.selektirani.push(kamen);
        kamen.selektiraj(nacin);
    }

    deselektirajKamen(kamen: Kamen): void {
        var index = this.selektirani.indexOf(kamen);
        this.selektirani.splice(index, 1);
        kamen.deselektiraj();
    }

    nacrtajString(s: string): void {
        s = s.replace(/[^XxOo\-\_]/g, '');

        var brojX = 0;
        var brojO = 0;

        for (var i = 0; i < Math.min(this.polja.length, s.length); i++) {
            this.polja[i].boja = s[i];
            if (s[i] == 'x' || s[i] == 'X') {
                brojX++;
            }
            if (s[i] == 'o' || s[i] == 'O') {
                brojO++;
            }
        }

        brojX = 14 - brojX;
        brojO = 14 - brojO;

        var HTMLizguraniX = document.getElementById("izgurani-x");
        var HTMLizguraniO = document.getElementById("izgurani-o");

        if(HTMLizguraniX != null && HTMLizguraniO !== null) {
          HTMLizguraniX.innerHTML = '';
          HTMLizguraniO.innerHTML = '';

          for (var i = 0; i < brojX; i++) HTMLizguraniX.innerHTML += '<div class="polje polje-x"></div>';
          for (var i = 0; i < brojO; i++) HTMLizguraniO.innerHTML += '<div class="polje polje-o"></div>';
        }

        this.nacrtaj();
    }

    toString(): string {
        var s: string = "";
        for (let i = 0; i < this.polja.length; i++) {
            s += this.polja[i].boja;
        }
        return s;
    }

    izracunajHeuristike(f1x:number = 1, f1o:number = 1, f2x:number = 1, f2o:number = 1, f3x:number = 1, f3o:number = 1, f4x:number = 1, f4o:number = 1) {
        var data: string;
        var znaci = ["x", "o"];
        for (let i = 0; i < znaci.length; i++) {
            var znak = znaci[i];
            data = '("' + znak + '" "' + this.toString() + '" ' + f1x + ' ' + f1o + ' ' + f2x + ' ' + f2o + ' ' + f3x + ' ' + f3o + ' ' + f4x + ' ' + f4o + ')';
            smackjack.heuristika(data, function(response) {
                var nizHeuristika: Array<number> = response.match(/[-+]?[0-9]*\.?[0-9]+/g);
                console.log(nizHeuristika);
            }, null);
        }
    }
}
