/// <reference path="./server.ts"/>
/// <reference path="./abalone.ts"/>

var tabla;


function posaljiPotez(kameni, smer): any {
    if (document.getElementsByClassName('selektiran-lose').length > 0)
        return;

    var sel = tabla.selektirani;
    var string = '';
    for (var i = 0; i < sel.length; i++) {
        string += '(' + sel[i].koordinata.x + ' ' + sel[i].koordinata.y + ' ' + sel[i].koordinata.z + ') ';
    }
    string = "(" + string + ") ";
    string = string + smer + ' *tabla*';


    //var string = 'odigrajPotez (caar d) (cadar d) *tabla*';
    //alert(string);
    smackjack.echo(string, callback, null);
    smackjack.heuristikaAJAX('x', prikaziHeuristike, null);
    smackjack.heuristikaAJAX('o', prikaziHeuristike, null);
}

window.onload = function() {
    tabla = new Tabla(5);
    tabla.nacrtaj();
    smackjack.reset('', callback, null);

    // Event listeneri za smerove
    document.getElementById('smer1').addEventListener('click', function() { posaljiPotez(tabla.selektirani, 1); });
    document.getElementById('smer2').addEventListener('click', function() { posaljiPotez(tabla.selektirani, 2); });
    document.getElementById('smer3').addEventListener('click', function() { posaljiPotez(tabla.selektirani, 3); });
    document.getElementById('smer4').addEventListener('click', function() { posaljiPotez(tabla.selektirani, 4); });
    document.getElementById('smer5').addEventListener('click', function() { posaljiPotez(tabla.selektirani, 5); });
    document.getElementById('smer6').addEventListener('click', function() { posaljiPotez(tabla.selektirani, 6); });

    var tablaHTML = document.getElementById('tabla-id');
    tablaHTML.classList.add('axial-visible');

    var radioKoord: HTMLInputElement[] = (<HTMLInputElement[]><any>document.getElementsByName('radio-koordinate'));

    document.getElementsByName('prikazi-koordinate')[0].addEventListener('change', function() {
        tablaHTML.classList.toggle('sakrij-koordinate');
        for (var i = 0; i < radioKoord.length; i++) { radioKoord[i].disabled = !this.checked }
    });

    document.getElementsByName("strict-mode")[0].addEventListener('change', function() {
        if (!this.checked) {
            var crveni = document.getElementsByClassName('selektiran-lose');
            for (let i = 0; i < crveni.length; i++) {
                crveni[i].classList.remove('selektiran-lose');
                crveni[i].classList.add('selektiran-dobro');
            }
        }
        else {
            for (let i = 0; i < tabla.selektirani.length; i++) {
                var pom = tabla.selektirani[0];
                tabla.selektirani[0].deselektiraj();
                tabla.selektirani.splice(0, 1);
                pom._onclick();
            }
        }
    });

    radioKoord[0].addEventListener('change', function(event) {
        tablaHTML.classList.toggle('axial-visible');
    });
    radioKoord[1].checked = true;
    radioKoord[1].addEventListener('change', function(event) {
        tablaHTML.classList.toggle('axial-visible');
    });

    document.getElementById("config-button").addEventListener('click', function() {
        document.getElementById("config").classList.toggle("hidden");
    });

    document.addEventListener("mouseup", function() {
        isMouseDown = false;
    });

}

document.onkeypress = function(e) {
    var c = e.keyCode;
    if (c == 100 || c == 54) posaljiPotez(tabla.selektirani, 1);   // d, 6
    if (c == 101 || c == 57) posaljiPotez(tabla.selektirani, 2);   // e, 9
    if (c == 119 || c == 55) posaljiPotez(tabla.selektirani, 3);   // w, 7
    if (c == 97 || c == 52) posaljiPotez(tabla.selektirani, 4);   // a, 4
    if (c == 122 || c == 121 || c == 49) posaljiPotez(tabla.selektirani, 5);   // z, y, 1
    if (c == 120 || c == 51) posaljiPotez(tabla.selektirani, 6);   // x, 3
    if (c == 115 || c == 53) deselektirajSve(); // s, 5
}

function deselektirajSve() {
    for (let i = 0; i < tabla.selektirani.length; i++) {
        tabla.selektirani[i].deselektiraj();
    }
    tabla.selektirani = [];
}

function callback(response) {
    console.log(response);
    if (tabla.poslednjeStanje !== null && tabla.poslednjeStanje != response) {
        document.getElementById("stats-x").classList.toggle("trenutni-na-redu");
        document.getElementById("stats-o").classList.toggle("trenutni-na-redu");
    }
    tabla.poslednjeStanje = response;
    return tabla.nacrtajString(response);
};

function prikaziHeuristike(response) {
    // TODO dodaj heuristike
    console.log(response);
}

/*function onClick() {
    return smackjack.echo((<HTMLInputElement>document.getElementById("data")).value, callback, null);
};*/
