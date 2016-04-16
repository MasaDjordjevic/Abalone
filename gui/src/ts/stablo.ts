/// <reference path="./server.ts"/>
/// <reference path="./abalone.ts"/>
/// <reference path="./dt/jquery/jquery.d.ts" />

var table = [];

window.onload = function() {
    var inicijalneTable: Array<string> = new Array<string>();


    inicijalneTable.push("xx-oo  xxxooo  -xx-oo-  --------  ---------  --------  -oo-xx-  oooxxx  oo-xx"); // Belgian Daisy

    var t = true;
    if (t) {
        inicijalneTable.push("-----  ------  -------  --------  ---------  --------  -------  ------  -----"); // prazna tabla

        // Otvaranja
        inicijalneTable.push("-----  xx--oo  xxx-ooo  -xx--oo-  ---------  -oo--xx-  ooo-xxx  oo--xx  -----"); // German Daisy
        inicijalneTable.push("ooooo  oooooo  --ooo--  --------  ---------  --------  --xxx--  xxxxxx  xxxxx"); // Standard

        inicijalneTable.push("x----  x-----  x------  x-------  x--------  x-------  x------  x----x  xxxxx"); // najdalje od centra

        inicijalneTable.push("-----  ------  -------  ---xx---  --xxxxx--  --xxxx--  --xxx--  ------  -----"); // najblize centru sa 14 perli [izgurano 0]
        inicijalneTable.push("-----  ------  -------  ---xx---  ---xxxx--  --xxxx--  --xxx--  ------  -----"); // najblize centru sa 13 perli [izgurano 1]
        inicijalneTable.push("-----  ------  -------  ---xx---  ---xxxx--  ---xxx--  --xxx--  ------  -----"); // najblize centru sa 12 perli [izgurano 2]
        inicijalneTable.push("-----  ------  -------  ---xx---  ---xxxx--  ---xxx--  ---xx--  ------  -----"); // najblize centru sa 11 perli [izgurano 3]
        inicijalneTable.push("-----  ------  -------  ---xx---  ---xxxx--  ---xxx--  ---x---  ------  -----"); // najblize centru sa 10 perli [izgurano 4]
        inicijalneTable.push("-----  ------  -------  ---xx---  ---xxx---  ---xxx--  ---x---  ------  -----"); // najblize centru sa 9 perli [izgurano 5]
        inicijalneTable.push("-----  ------  -------  ---xx---  ---xxx---  ---xx---  ---x---  ------  -----"); // najblize centru sa 8 perli [izgurano svih 6]

        //inicijalneTable.push("x----  ------  -------  --------  ---------  --------  -------  ------  ----x"); // katastrofa grupisanje sa 2 perle
        //inicijalneTable.push("x---x  ------  -------  --------  x---x---x  --------  -------  ------  x---x"); // katastrofa grupisanje sa 7 perli
        inicijalneTable.push("x---x  ------  --x----  --------  x---x---x  --------  ----x--  ------  x---x"); // katastrofa grupisanje sa 9 perli [izgubljeno 5]
        inicijalneTable.push("x---x  ---x--  --x----  -----x--  x---x---x  --x-----  ----x--  --x---  x---x"); // katastrofa grupisanje sa 13 perli [izgubljena 1]

        inicijalneTable.push("-----  ------  -------  --------  ----x----  --------  -------  ------  -----"); // fenomenalno grupisanje sa 1 perlom
        inicijalneTable.push("xx---  ------  -------  --------  ---------  --------  -------  ------  -----"); // fenomenalno grupisanje sa 2 perle
        inicijalneTable.push("xx---  -x----  -------  --------  ---------  --------  -------  ------  -----"); // fenomenalno grupisanje sa 3 perle
        inicijalneTable.push("xx---  xx----  -------  --------  ---------  --------  -------  ------  -----"); // fenomenalno grupisanje sa 4 perle
        inicijalneTable.push("xx---  xxx---  -------  --------  ---------  --------  -------  ------  -----"); // fenomenalno grupisanje sa 5 perli
        inicijalneTable.push("xx---  xxx---  -x-----  --------  ---------  --------  -------  ------  -----"); // fenomenalno grupisanje sa 6 perli
        inicijalneTable.push("xx---  x-x---  -xx----  --------  ---------  --------  -------  ------  -----"); // fenomenalno grupisanje sa 6 perli

        inicijalneTable.push("-----  ------  -------  ---xx---  --xxxxx--  --xxxx--  --xxx--  ------  -----"); // dobro grupisanje sa 14 perli [izgurano 0]
        inicijalneTable.push("-----  ------  -------  ---xx---  ---xxx---  ---xxx--  ---x---  ------  -----"); // dobro grupisanje sa 9 perli [izgurano 5]

        inicijalneTable.push("-----  -x---o  --oxxo-  --oox---  --oxxxxo-  ---xxxo-  --oxoox  --o-o-  -o-x-"); // neka tabla
    }

    for (let i = 0; i < inicijalneTable.length; i++)
        kreirajTablu(document.getElementsByClassName("tabla-container")[0], inicijalneTable[i]);
}

function kreirajTablu(cont, str) {
  var li = document.createElement("li");
  var wrapper = document.createElement("div");
  wrapper.className = "item";

  var el = document.createElement("div");
  el.classList.add("tabla");
  el.classList.add("sakrij-koordinate");

  var tabla = new TablaZaCrtanje(5, el, 15);
  tabla.nacrtajString(str);
  tabla.izracunajHeuristike(999999, 999999, 1, 1, 1, 1, 1, 1);
  tabla.div.addEventListener("click", function(){
    window.prompt("Copy to clipboard: Ctrl+C, Enter", str);
  })
  table.push(tabla);
  wrapper.appendChild(el);

  var span = document.createElement("span");
  span.className = "string";
  wrapper.appendChild(span);

  var btn = document.createElement("button");
  btn.className = "toggle-descendants";
  btn.innerHTML = "-";
  btn.onclick = function() {
    this.innerHTML = this.innerHTML == "-" ? "+" : "-";
    $(this).parent().siblings().toggleClass("hidden");
  }
  wrapper.appendChild(btn);

  var btn = document.createElement("button");
  btn.className = "fetch-children-x";
  //btn.innerHTML = "x";
  btn.onclick = function () {
    _onclick(this, "x", tabla);
  }
  wrapper.appendChild(btn);

  var btn = document.createElement("button");
  btn.className = "fetch-children-o";
  //btn.innerHTML = "o";
  btn.onclick = function() {
    _onclick(this, "o", tabla);
  }
  wrapper.appendChild(btn);
  var s:string = '';
  //s += '<div class="heuristics">';
    s += '<dl class="h-pobeda-ja">    <dt>Pobeda Ja</dt>     <dd>?</dd></dl>';
    s += '<dl class="h-pobeda-on">    <dt>Pobeda On</dt>     <dd>?</dd></dl>';
    s += '<dl class="h-izgurani-ja">  <dt>Izgurani Ja</dt>   <dd>?</dd></dl>';
    s += '<dl class="h-izgurani-on">  <dt>Izgurani On</dt>   <dd>?</dd></dl>';
    s += '<dl class="h-centar-ja">    <dt>Centar Ja</dt>     <dd>?</dd></dl>';
    s += '<dl class="h-centar-on">    <dt>Centar On</dt>     <dd>?</dd></dl>';
    s += '<dl class="h-grupisanje-ja"><dt>Grupisanje Ja</dt> <dd>?</dd></dl>';
    s += '<dl class="h-grupisanje-on"><dt>Grupisanje On</dt> <dd>?</dd></dl>';
    s += '<dl class="h-ukupno">       <dt>UKUPNO</dt>        <dd>?</dd></dl>';
  //s += '</div>';
  $(wrapper).append(jQuery.parseHTML('<div class="heuristics heuristics-x">' + s + '</div>'));
  $(wrapper).append(jQuery.parseHTML('<div class="heuristics heuristics-o">' + s + '</div>'));

  li.appendChild(wrapper);
  cont.appendChild(li);
}

function _onclick(btn:HTMLButtonElement, znak:string, tabla: TablaZaCrtanje) {
  btn.disabled = true;
  var ul = document.createElement("ul");
  ul.className = "tabla-container contains-" + znak;
  var tablaSelf = tabla;

  //ajax deca
  var decaStr = smackjack.deca(znak + tabla.toString(), function(response) {
    var res:string = response.replace(/[^XxOo\-\_\"]/g, '');
    res = res.substr(2, res.length - 4); // sklanja vodeca dva i krajnja dva navodnika
    var stanja = res.split('""""'); // delimeter izmedju stanja
    for (var i = 0; i < stanja.length; i++) {
       kreirajTablu(ul, stanja[i]);
    }
  }, null)

  btn.parentNode.parentNode.appendChild(ul);
  (<HTMLButtonElement>btn.parentNode.parentNode).classList.add("expanded");
}

function randomTabla() {
  var str = "";
  var znaci = ["x", "o", "-"];
  for (var i = 0; i < 61; i++) {
    str += znaci[Math.floor((Math.random() * 100) % 3)];
  }
  return str;
}
