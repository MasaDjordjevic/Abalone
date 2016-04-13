/// <reference path="./server.ts"/>
/// <reference path="./abalone.ts"/>
/// <reference path="./dt/jquery/jquery.d.ts" />

var table = [];

window.onload = function() {
  kreirajTablu(document.getElementsByClassName("tabla-container")[0]);


}

function kreirajTablu(cont) {
  var li = document.createElement("li");
  var wrapper = document.createElement("div");
  wrapper.className = "item";

  var el = document.createElement("div");
  el.classList.add("tabla");
  el.classList.add("sakrij-koordinate");

  var tabla = new Tabla(5, Igrac.Human, Igrac.Human, el, 15, false);
  tabla.nacrtajString(randomTabla());
  table[table.length++] = tabla;
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
  btn.innerHTML = "x";
  btn.onclick = function () {
    _onclick(this, "x");
  }
  wrapper.appendChild(btn);

  var btn = document.createElement("button");
  btn.className = "fetch-children-o";
  btn.innerHTML = "o";
  btn.onclick = function() {
    _onclick(this, "o");
  }
  wrapper.appendChild(btn);

  li.appendChild(wrapper);
  cont.appendChild(li);
}

function _onclick(btn:HTMLButtonElement, znak:string) {
  btn.disabled = true;
  var ul = document.createElement("ul");
  ul.className = "tabla-container contains-" + znak;
   for (let i = 0; i < 20; i++) {
    kreirajTablu(ul);
   }
   btn.parentNode.parentNode.appendChild(ul);
   (<HTMLButtonElement>btn.parentNode.parentNode).classList.add("expanded");
}

function randomTabla() {
  var str = "";
  var znaci = ["x", "o", "-"];
  for(var i = 0; i < 61; i++) {
    str += znaci[Math.floor((Math.random() * 100) % 3)];
  }

  return str;
}
