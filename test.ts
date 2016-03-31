var tabla;

function posaljiPotez(kameni, smer): any {
    var sel = tabla.selektirani;
    var string = '';
    for (var i = 0; i < sel.length; i++) {
        string += '(' + sel[i].koordinata.x + ' ' + sel[i].koordinata.y + ' ' + sel[i].koordinata.z + ') ';
    }
    string = '(' + string + ')'; // lista koordinata
    string += ' ' + smer;
    string = '(' + string + ')'; // konacno okruzivanje zagradama za poziv
    //alert(string);
    // PROBLEM'S
}

window.onload = function() {
    tabla = new Tabla(5);
    tabla.nacrtaj();

    // Event listeneri za smerove
    document.getElementById('smer1').addEventListener('click', function() {posaljiPotez(tabla.selektirani, 1)});
    document.getElementById('smer2').addEventListener('click', function() {posaljiPotez(tabla.selektirani, 2)});
    document.getElementById('smer3').addEventListener('click', function() {posaljiPotez(tabla.selektirani, 3)});
    document.getElementById('smer4').addEventListener('click', function() {posaljiPotez(tabla.selektirani, 4)});
    document.getElementById('smer5').addEventListener('click', function() {posaljiPotez(tabla.selektirani, 5)});
    document.getElementById('smer6').addEventListener('click', function() {posaljiPotez(tabla.selektirani, 6)});
}

class Koordinata {
    constructor (public x: number = 0, public y: number = 0, public z: number = 0) { }
}

class Kamen {
    div: HTMLElement;

    constructor(
        public tabla: Tabla,
        public koordinata: Koordinata = new Koordinata(),
        public boja: string = '-'
    ) { }

    stampaj(): void {
        var kamen = document.createElement('div');

        // koji je znak na kamenu
        kamen.classList.add('polje');
        kamen.classList.add('polje-' + this.boja);

        // odredjivanje pozicije
        var sqrt3 = Math.sqrt(3.0);
        var size = 55;

        var x = this.koordinata.x * size;
        var y = this.koordinata.y * size;
        var z = this.koordinata.z * size;

        var left = x + (z - (size - (size*sqrt3)/2) * (this.koordinata.z % 2)) / 2;
        var top  = z;

        top  += 250;
        left += 250;

        kamen.style.top = top.toString() + 'px';
        kamen.style.left = left.toString() + 'px';

        // upis u kamen
        kamen.innerHTML = '<span class="koordinate">' + this.koordinata.x + ', ' + this.koordinata.y + ', ' + this.koordinata.z + '</span>';

        document.getElementById("tabla-id").appendChild(kamen);
        this.div = kamen;
        this.div.onclick = (e) => {
            this._onclick();
        }
    }

    _onclick() {
        this.tabla.toggleKamen(this);
    }
}

class Tabla {
    polja: Kamen[];
    selektirani: Kamen[];

    constructor(velicina: number) {
        this.polja = new Array<Kamen>(3 * velicina * velicina - 3 * velicina + 1); // 3n² - 3n + 1
        for (var i = 0; i < this.polja.length; i++) {
            this.polja[i] = new Kamen(this);
        }

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
    }

    nacrtaj(): void {
        this.selektirani = [];
        document.getElementById('selektirani').innerHTML = '';
        document.getElementById('tabla-id').innerHTML = '';
        for (var i = 0; i < this.polja.length; i++) {
            this.polja[i].stampaj();
        }
    }

    toggleKamen(kamen: Kamen): void {
        var index = this.selektirani.indexOf(kamen);
        if (index == -1) {
            // nema ga
            this.selektirajKamen(kamen);
        } else {
            // ima ga
            this.deselektirajKamen(kamen);
        }

        // Apdejtovanje DIV-a
        var string = '';
        for (var i = 0; i < tabla.selektirani.length; i++) {
            string += '<li>';
            string += tabla.selektirani[i].koordinata.x + ', ' + tabla.selektirani[i].koordinata.y + ', ' + tabla.selektirani[i].koordinata.z;
            string += '</li>';
        }
        document.getElementById('selektirani').innerHTML = string;
    }

    selektirajKamen(kamen: Kamen): void {
        // var size = this.selektirani.filter(function(value) {return value !== undefined}).length;
        if (this.selektirani.length >= 3) {
            this.selektirani[0].div.classList.remove('selektiran');
            this.selektirani.splice(0, 1);
        }
        this.selektirani.push(kamen);
        kamen.div.classList.add('selektiran');
    }

    deselektirajKamen(kamen: Kamen): void {
        var index = this.selektirani.indexOf(kamen);
        this.selektirani.splice(index, 1);
        kamen.div.classList.remove('selektiran');
    }

    nactajString(s: string): void {
        s = s.replace(/\s/g, ''); // skloni sve beline
        for (var i = 0; i < Math.min(tabla.polja.length, s.length); i++) {
            tabla.polja[i].boja = s[i];
        }
        tabla.nacrtaj();
    }
}
