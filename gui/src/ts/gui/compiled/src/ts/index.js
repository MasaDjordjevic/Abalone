var tabla;
function posaljiPotez(kameni, smer) {
    var sel = tabla.selektirani;
    var string = '';
    for (var i = 0; i < sel.length; i++) {
        string += '(' + sel[i].koordinata.x + ' ' + sel[i].koordinata.y + ' ' + sel[i].koordinata.z + ') ';
    }
    string = '(' + string + ')';
    string += ' ' + smer;
    string = '(' + string + ')';
}
window.onload = function () {
    tabla = new Tabla(5);
    tabla.nacrtaj();
    document.getElementById('smer1').addEventListener('click', function () { posaljiPotez(tabla.selektirani, 1); });
    document.getElementById('smer2').addEventListener('click', function () { posaljiPotez(tabla.selektirani, 2); });
    document.getElementById('smer3').addEventListener('click', function () { posaljiPotez(tabla.selektirani, 3); });
    document.getElementById('smer4').addEventListener('click', function () { posaljiPotez(tabla.selektirani, 4); });
    document.getElementById('smer5').addEventListener('click', function () { posaljiPotez(tabla.selektirani, 5); });
    document.getElementById('smer6').addEventListener('click', function () { posaljiPotez(tabla.selektirani, 6); });
};
var Koordinata = (function () {
    function Koordinata(x, y, z) {
        if (x === void 0) { x = 0; }
        if (y === void 0) { y = 0; }
        if (z === void 0) { z = 0; }
        this.x = x;
        this.y = y;
        this.z = z;
    }
    return Koordinata;
}());
var Kamen = (function () {
    function Kamen(tabla, koordinata, boja) {
        if (koordinata === void 0) { koordinata = new Koordinata(); }
        if (boja === void 0) { boja = '-'; }
        this.tabla = tabla;
        this.koordinata = koordinata;
        this.boja = boja;
    }
    Kamen.prototype.stampaj = function () {
        var _this = this;
        var kamen = document.createElement('div');
        kamen.classList.add('polje');
        kamen.classList.add('polje-' + this.boja);
        var sqrt3 = Math.sqrt(3.0);
        var size = 55;
        var x = this.koordinata.x * size;
        var y = this.koordinata.y * size;
        var z = this.koordinata.z * size;
        var left = x + (z - (size - (size * sqrt3) / 2) * (this.koordinata.z % 2)) / 2;
        var top = z;
        top += 250;
        left += 250;
        kamen.style.top = top.toString() + 'px';
        kamen.style.left = left.toString() + 'px';
        kamen.innerHTML = '<span class="koordinate">' + this.koordinata.x + ', ' + this.koordinata.y + ', ' + this.koordinata.z + '</span>';
        document.getElementById("tabla-id").appendChild(kamen);
        this.div = kamen;
        this.div.onclick = function (e) {
            _this._onclick();
        };
    };
    Kamen.prototype._onclick = function () {
        this.tabla.toggleKamen(this);
    };
    return Kamen;
}());
var Tabla = (function () {
    function Tabla(velicina) {
        this.polja = new Array(3 * velicina * velicina - 3 * velicina + 1);
        for (var i = 0; i < this.polja.length; i++) {
            this.polja[i] = new Kamen(this);
        }
        var index = 0;
        for (var z = -(velicina - 1); z <= (velicina - 1); z++) {
            var duzinaReda = 2 * velicina - 1 - Math.abs(z);
            var x, y;
            if (z <= 0) {
                y = velicina - 1;
                x = 0 - z - y;
            }
            else {
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
    Tabla.prototype.nacrtaj = function () {
        this.selektirani = [];
        document.getElementById('selektirani').innerHTML = '';
        document.getElementById('tabla-id').innerHTML = '';
        for (var i = 0; i < this.polja.length; i++) {
            this.polja[i].stampaj();
        }
    };
    Tabla.prototype.toggleKamen = function (kamen) {
        var index = this.selektirani.indexOf(kamen);
        if (index == -1) {
            this.selektirajKamen(kamen);
        }
        else {
            this.deselektirajKamen(kamen);
        }
        var string = '';
        for (var i = 0; i < tabla.selektirani.length; i++) {
            string += '<li>';
            string += tabla.selektirani[i].koordinata.x + ', ' + tabla.selektirani[i].koordinata.y + ', ' + tabla.selektirani[i].koordinata.z;
            string += '</li>';
        }
        document.getElementById('selektirani').innerHTML = string;
    };
    Tabla.prototype.selektirajKamen = function (kamen) {
        if (this.selektirani.length >= 3) {
            this.selektirani[0].div.classList.remove('selektiran');
            this.selektirani.splice(0, 1);
        }
        this.selektirani.push(kamen);
        kamen.div.classList.add('selektiran');
    };
    Tabla.prototype.deselektirajKamen = function (kamen) {
        var index = this.selektirani.indexOf(kamen);
        this.selektirani.splice(index, 1);
        kamen.div.classList.remove('selektiran');
    };
    Tabla.prototype.nactajString = function (s) {
        s = s.replace(/\s/g, '');
        for (var i = 0; i < Math.min(tabla.polja.length, s.length); i++) {
            tabla.polja[i].boja = s[i];
        }
        tabla.nacrtaj();
    };
    return Tabla;
}());
