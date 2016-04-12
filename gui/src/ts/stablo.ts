/// <reference path="./server.ts"/>
/// <reference path="./abalone.ts"/>
/// <reference path="./dt/jquery/jquery.d.ts" />

var table = [];

window.onload = function() {
  
  kreirajTablu(document.getElementsByClassName("tabla-container")[0]);
  for (let i = 0; i < 7; i++) {
    kreirajTablu(document.getElementsByClassName("tabla-container")[1]);
  };

}

function kreirajTablu(cont) {
  var wrapper = document.createElement("div");
  wrapper.className = "item";

  var el = document.createElement("div");
  el.classList.add("tabla");
  el.classList.add("sakrij-koordinate");

  var tabla = new Tabla(5, Igrac.Human, Igrac.Human, el, 25, false);
  tabla.nacrtajString(randomTabla());
  table[table.length++] = tabla;
  wrapper.appendChild(el);

  var span = document.createElement("span");
  span.className = "string";
  wrapper.appendChild(span);

  var btn = document.createElement("button");
  btn.className = "toggle-descendants";
  btn.innerHTML = "-";
  wrapper.appendChild(btn);

  var btn = document.createElement("button");
  btn.className = "fetch-children-x";
  btn.innerHTML = "x";
  wrapper.appendChild(btn);

  var btn = document.createElement("button");
  btn.className = "fetch-children-o";
  btn.innerHTML = "o";
  wrapper.appendChild(btn);

  cont.appendChild(wrapper);
}

function randomTabla() {
  var str = "";
  var znaci = ["x", "o", "-"];
  for(var i = 0; i < 61; i++) {
    str += znaci[Math.floor((Math.random() * 100) % 3)];
  }

  return str;
}
