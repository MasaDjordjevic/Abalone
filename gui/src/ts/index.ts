/// <reference path="./server.ts"/>
/// <reference path="./abalone.ts"/>

var tabla;

function posaljiPotez(kameni, smer): any {
    if (document.getElementsByClassName('selektiran-lose').length > 0)
        return;

    var sel = tabla.selektirani;
    var string = '';
    for (var i = 0; i < sel.length; i++) {
        string += '(' + sel[i].koordinata.getAxialCoordinates()[0] + ' ' + sel[i].koordinata.getAxialCoordinates()[1] + ') ';
        // string += '(' + sel[i].koordinata.x + ' ' + sel[i].koordinata.y + ' ' + sel[i].koordinata.z + ') ';
    }
    string = "(" + string + ") ";
    string = string + smer + ' *tabla*';

    smackjack.potez(string, callback, null);
    // smackjack.echo(string, callback, null);
}

var novaIgra = function() {
    var prvi = (<HTMLInputElement>document.getElementById('igrac-1-human')).checked ? Igrac.Human : Igrac.AI;
    var drugi = (<HTMLInputElement>document.getElementById('igrac-2-human')).checked ? Igrac.Human : Igrac.AI;

    tabla = new Tabla(5, prvi, drugi, document.getElementById('tabla-id'), 70);

    smackjack.reset('', callback, null);
}


window.onload = function() {

    var prvi = Igrac.Human;
    var drugi = Igrac.Human;

    (<HTMLInputElement>document.getElementById('igrac-1-human')).checked = (prvi == Igrac.Human);
    (<HTMLInputElement>document.getElementById('igrac-1-ai')).checked = (prvi == Igrac.AI);
    (<HTMLInputElement>document.getElementById('igrac-2-human')).checked = (drugi == Igrac.Human);
    (<HTMLInputElement>document.getElementById('igrac-2-ai')).checked = (drugi == Igrac.AI);

    novaIgra();
    document.getElementById("nova-igra").addEventListener('click', novaIgra);

    // Event listeneri za smerove
    document.getElementById('smer1').addEventListener('click', function() { posaljiPotez(tabla.selektirani, 1); });
    document.getElementById('smer2').addEventListener('click', function() { posaljiPotez(tabla.selektirani, 2); });
    document.getElementById('smer3').addEventListener('click', function() { posaljiPotez(tabla.selektirani, 3); });
    document.getElementById('smer4').addEventListener('click', function() { posaljiPotez(tabla.selektirani, 4); });
    document.getElementById('smer6').addEventListener('click', function() { posaljiPotez(tabla.selektirani, 6); });
    document.getElementById('smer5').addEventListener('click', function() { posaljiPotez(tabla.selektirani, 5); });


    var tablaHTML = document.getElementById('tabla-id');
    tablaHTML.classList.add('axial-visible');

    var radioKoord: HTMLInputElement[] = (<HTMLInputElement[]><any>document.getElementsByName('radio-koordinate'));

    document.getElementsByName('prikazi-koordinate')[0].addEventListener('change', function() {
        tablaHTML.classList.toggle('sakrij-koordinate');
        for (var i = 0; i < radioKoord.length; i++) { radioKoord[i].disabled = !this.checked }
    });

    document.getElementsByName("strict-mode")[0].addEventListener('change', function() {
        if (!this.checked) {
            /*
            var crveni = document.getElementsByClassName('selektiran-lose');
            var lol = "haha";
            for (let i = 0; i < crveni.length; i++) {
                //crveni[i].classList.remove("selektiran-lose");//.replace("selektiran-lose", "");className.replace("selektiran-lose", "")
                //crveni[i].className += 'selektiran-dobro';

            }
            */
            $('.selektiran-lose').addClass("selektiran-dobro").removeClass("selektiran-lose");
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

    // Select on drag
    document.addEventListener("mouseup", function() {
        isMouseDown = false;
    });

    // Window on load
    // Slanje heuristika
    $(document).on('keypress keydown keyup', '#h-igrac-1, #h-igrac-2', function(e) {
        var $self = $(this);
        if (e.keyCode != 13) // Enter
        $(this).addClass('unsaved');
        else {
            var h = $('#h-igrac-1').val() + ' ' + $('#h-igrac-2').val();
            h = h.replace(/\s+/g, " ");
            if (h.split(' ').length != 12) {
                alert('Pogresan unos!');
                debugger;
            }
            smackjack.promeniHeuristike(h, function(response) {
                $self.removeClass("unsaved");
            }, null);
        }
    });
}

// Shortcutovi na tastaturi za pomeranje kad igra Human
document.onkeypress = function(e) {
    if ($('.h-parametri > input').is(":focus")) return;
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
    if (tabla.poslednjeStanje !== null && tabla.poslednjeStanje != response.replace(/[^XxOo\-\_]/g, '')) {
        tabla.toggleNaRedu();
    }
    tabla.setPoslednjeStanje(response);
    tabla.nacrtajString(response);

    var naRedu = tabla.igraci[tabla.naRedu];
    var znakIgracaNaRedu = tabla.naRedu == 0 ? "x" : "o";
    console.log("Potez treba da odigra " + (naRedu == Igrac.Human ? "čovek" : "AI") + ".");

    if (naRedu == Igrac.AI) {
        console.log("AI-ju se postavlja zadatak!\nTabla je:\n"  + tabla.poslednjeStanje);
        setTimeout(function() {
            smackjack.AIodigrajPotez(znakIgracaNaRedu + tabla.poslednjeStanje, callback, null);
        }, 500);
    }
};
