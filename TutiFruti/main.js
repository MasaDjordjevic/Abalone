"use strict";

var receivedData = null;
var _stop = false;
var _nextPlayer = 1;
var log = true;

window.onload = function() {
    drawGenerator();

    smackjack.reset("", parse, null);

    $("#example-xo").click(function(){
        smackjack.exampleXo("", parse, null);
    });

    $("#example-chess").click(function () {
        smackjack.exampleChess("", parse, null);
    });

    $("#example-hexa-chess").click(function () {
        smackjack.exampleHexaChess("", parse, null);
    });

    $("#example-gomoku-eastern").click(function () {
        smackjack.exampleGomokuEastern("", parse, null);
    });

    $("#example-gomoku-western").click(function () {
        smackjack.exampleGomokuWestern("", parse, null);
    });

    $("#odigraj-potez-1").click(function () {
        var port = $("#port-1").val();
        smackjack.odigrajPotez(JSON.stringify(receivedData), parse, null, port);
    });

    $("#reset-1").click(function () {
        var port = $("#port-1").val();
        smackjack.reset(JSON.stringify(receivedData), parse, null, port);
    });

    $("#odigraj-potez-2").click(function () {
        var port = $("#port-2").val();
        smackjack.odigrajPotez(JSON.stringify(receivedData), parse, null, port);
    });

    $("#reset-2").click(function () {
        var port = $("#port-2").val();
        smackjack.reset(JSON.stringify(receivedData), parse, null, port);
    });

    var play = function() {
        _stop = false;
        odigraj();
    };
    $("#igraj").click(play);
    $(document).bind("keyup", "i", play);

    var stop = function() {
        _stop = true;
    };
    $("#stop").click(stop);
    $(document).bind("keyup", "p", stop);

    $(".generator form").submit(function() {
        generate();
        return false; // prevent page refresh on submit
    });
};

function parse(data) {
    receivedData = (JSON.parse((JSON.parse(data))));
    displayData(receivedData);
}

function odigraj() {
    if (log) {
        console.log("zovem " + _nextPlayer);
    }
    var port = $("#port-" + _nextPlayer).val();
    smackjack.odigrajPotez(JSON.stringify(receivedData), odigrajCallback, null, port);
}

function odigrajCallback(data) {
    receivedData = (JSON.parse((JSON.parse(data))));
    displayData(receivedData);

    if (log) {
        console.log("dobio sam od " + _nextPlayer);
        console.log(receivedData);
    }
    _nextPlayer = _nextPlayer == 1 ? 2 : 1;

    if (_stop) return;
    var delay = +$("#delay").val();
    setTimeout(odigraj, delay);
}


function generate() {
    var board = {};
    board.type = $("#gen_boardType").val();
    board.dimensions = [];
    board.dimensions[0] = +$("#gen_boardDimensions-1").val();
    board.dimensions[1] = +$("#gen_boardDimensions-2").val();
    board.dimensions[2] = +$("#gen_boardDimensions-3").val();
    board.corner = $("#gen_boardCorner").val();
    board.axis = [];
    board.axis[0] = $("#gen_boardAxis-1").val();
    board.axis[1] = $("#gen_boardAxis-2").val();
    board.coloring = $("#gen_boardColoring").val();
    board.mode = $("#gen_boardMode").val();
    board.size = $("#gen_boardSize").val();

    var player = {};
    player.name = $("#gen_playerName").val();
    player.order = +$("#gen_playerOrder").val();
    player.message = $("#gen_playerMessage").val();

    var state = {};
    state.color = $("#gen_stateColor").val();
    state.shape = $("#gen_stateShape").val();

    var state2 = {};
    state2.color = $("#gen_stateColor-2").val();
    state2.shape = $("#gen_stateShape-2").val();

    var ax = [];
    ax[0] = board.axis[0].split(' ');
    ax[1] = board.axis[1].split(' ');
    var rand = [];
    var rand2 = [];
    for (let i = 0; i < Math.min(...board.dimensions); i++) {
        var a = ax[0][Math.floor(Math.random() * board.dimensions[0])];
        var b = ax[1][Math.floor(Math.random() * board.dimensions[1])];
        if (i % 2 == 0)
            rand.push([a, b]);
        else
            rand2.push([a, b]);

    }

    var data = {
        board: board,
        player: player,
        state: [
            {
                // Crni pešaci
                fields: rand,
                style: state
            },
            {
                fields: rand2,
                style: state2
            }
        ],
        markings: [],
        removed: []
    };

    // Vizuelni prikaz
    // Mora prvo ovo, da bi prvo izvrsio validaciju i adekvatno izmenio objekat.
    displayData(data);

    // Sada je objekat potpuno ispravan, jer je gore validiran.

    // Lisp i JSON
    $("#lispCode").val(getLispCode(data));
    $("#jsonCode").val(JSON.stringify(data));
}

function getLispCode(data) {
    var lisp = "";
    lisp += '(setq _board \'(\n' +
        '\t(type . "' + data.board.type + '")\n' +
        '\t(dimensions . (' + data.board.dimensions.join(' ') + '))\n' +
        '\t(corner . "' + data.board.corner + '")\n' +
        '\t(axis . ("' + data.board.axis[0] + '" "' + data.board.axis[1] + '"))\n' +
        '\t(mode . "' + data.board.mode + '")\n' +
        '\t(coloring . "' + data.board.coloring + '")\n' +
        '\t(size . "' + data.board.size + '")))\n' +
        '(setq _player \'(\n' +
        '\t(name . "' + data.player.name + '")\n' +
        '\t(order . ' + data.player.order + ')\n' +
        '\t(message . "' + data.player.message + '")))\n' +
        '(setq _state \'(\n';
    for (let i = 0; i < data.state.length; i++) {
        lisp +=
            '\t(\n' +
            '\t\t(fields . ((' +
            data.state[i].fields.map(el => '"' + el[0] + '" "' + el[1] + '"').join(")(") + ')))\n' +
            '\t\t(style . (\n' +
            '\t\t\t(color . "' + data.state[i].style.color + '")\n' +
            '\t\t\t(shape . "' + data.state[i].style.shape + '"))))\n';
    }
    lisp += "))\n";

    lisp += '(setq _markings \'(\n';
    for (let i = 0; i < data.markings.length; i++) {
        lisp +=
            '\t(\n' +
            '\t\t(fields . ((' +
            data.markings[i].fields.map(el => '"' + el[0] + '" "' + el[1] + '"').join(")(") + ')))\n' +
            '\t\t(style . (\n' +
            '\t\t\t(color . "' + data.markings[i].style.color + '")\n' +
            '\t\t\t(shape . "' + data.markings[i].style.shape + '"))))\n';
    }
    lisp += "))\n";

    if (data.removed[0].length != 0) {
        lisp += '(setq _removed \'(\n';
        for (let i = 0; i < data.removed.length; i++) {
            lisp +=
                '\t(\n' +
                '\t\t(fields . ' + data.removed[i].number + ')\n' +
                '\t\t(style . (\n' +
                '\t\t\t(color . "' + data.removed[i].style.color + '")\n' +
                '\t\t\t(shape . "' + data.removed[i].style.shape + '"))))\n';
        }
        lisp += "))";
    }

    return lisp;
}

var options = {};
options._boardType = ["rectangular", "hexagonal-flat", "hexagonal-pointy"];
options._boardDimensions = [15, 15, 6];
options._boardCorner = ["bottom-left", "top-left", "top-right", "bottom-right", "left", "right", "bottom", "top"];
options._boardAxis = [];
options._boardAxis[0] = "1 2 3 4 5 6 7 8 9 10 11 12 13 14 15";
options._boardAxis[1] = "A B C D E F G H I J K L M N O";
options._boardColoring = ["classic", "chess"];
options._boardMode = ["classic", "circles", "go"];
options._boardSize = ["l", "xxs", "xs", "s", "m", "xl", "xxl"];

options._playerName = "La Plavusha";
options._playerOrder = [1, 2];
options._playerMessage = "Zdravo deco";

options._stateColor = ["red", "pink", "purple", "deep-purple", "indigo", "blue", "light-blue", "cyan", "teal", "green",
    "light-green", "lime", "yellow", "amber", "deep-orange", "brown", "grey", "blue-grey", "black ", "white"];
options._stateShape = ["O", "X", "circle", "square", "star", "arrow-left", "arrow-right", "arrow-up", "arrow-down",
    "angle-left", "angle-right", "angle-up", "angle-down", "angle-double-left", "angle-double-right",
    "angle-double-up", "angle-double-down", "crosshairs", "bullseye", "check", "check-circle", "minus", "plus",
    "suit-spade-fill", "suit-spade-outline", "suit-heart-fill", "suit-heart-outline", "suit-diamond-fill",
    "suit-diamond-outline", "suit-club-fill", "suit-club-outline", "number-1", "number-2", "number-46", "number-53",
    "letter-A", "letter-B", "letter-M", "letter-a", "letter-s", "letter-q", "chess-king-outline", "chess-king-fill",
    "chess-queen-outline", "chess-queen-fill", "chess-rook-outline", "chess-rook-fill", "chess-bishop-outline",
    "chess-bishop-fill", "chess-knight-outline", "chess-knight-fill", "chess-pawn-outline", "chess-pawn-fill"];


function drawGenerator() {

    var $generator = $(".generator");
    var $generatorForm = $("<form/>", {
        id: "generator-form"
    })
        .appendTo($generator);
    var r = new RegExp("[A-Z].*");

    $.each(options, function (key, value) {
        var id = "gen" + key;
        var innerDiv = $("<div/>");
        var label = $("<label/>", {
            "for": id
        })
            .append(key.match(r))
            .appendTo(innerDiv);

        if (Object.prototype.toString.call(value) === '[object Array]') {
            switch (key) {
                case "_boardDimensions":
                    drawDimensions(id, value, innerDiv);
                    break;
                case "_boardAxis":
                    generateAxis(id, value, innerDiv);
                    break;
                default:
                    drawSelect(id, value, innerDiv);
            }
        } else if (typeof value == "string") {
            drawString(id, value, innerDiv);
        }

        $generatorForm.append(innerDiv);
    });

    var id, $innerDiv;

    id = "gen_stateColor-2";
    $innerDiv = $("<div/>");
    $("<label/>", {
        "for": id
    })
        .append("Color")
        .appendTo($innerDiv);
    drawSelect(id, options._stateColor, $innerDiv, 5);
    $generatorForm.append($innerDiv);

    id = "gen_stateShape-2";
    $innerDiv = $("<div/>");
    $("<label/>", {
        "for": id
    })
        .append("Shape")
        .appendTo($innerDiv);
    drawSelect(id, options._stateShape, $innerDiv, 13);
    $generatorForm.append($innerDiv);

    $("<button/>", {
        "type": "submit",
        "id": "gen-generate"
    })
        .append("Generisi")
        .appendTo($generatorForm);

    var $outputs = $("<div>", {
        id: "gen-outputs"
    });

    $("<div/>", {
        id: "jsonCodeOutput"
    })
        .append("<h3>JSON</h3>")
        .append($("<textarea/>", {
            readonly: "readonly",
            id: "jsonCode"
        }))
        .append($("<input/>", {
            type: "button",
            value: "Kopiraj",
            id: "jsonCopy"
        }))
        .appendTo($outputs);

    $("<div/>", {
        id: "lispCodeOutput"
    })
        .append("<h3>Lisp</h3>")
        .append($("<textarea/>", {
            readonly: "readonly",
            id: "lispCode"
        }))
        .append($("<input/>", {
            type: "button",
            value: "Kopiraj",
            id: "lispCopy"
        }))
        .appendTo($outputs);

    $outputs.appendTo($generatorForm);

}

function drawString(id, value, $container) {
    var input = $("<input/>", {
        "type": "text",
        "value": value,
        "id": id
    }).appendTo($container);
}

function drawDimensions(id, array, $container) {
    var $containerDiv = $("<div/>").appendTo($container);
    for (let i = 1; i <= 3; i++) {
        var label = $("<label/>", {
            "for": id + "-" + i
        }).append("D" + i).appendTo($containerDiv);
        var input = $("<input/>", {
            "id": id + "-" + i,
            "type": "number",
            "min": "1",
            "max": "20",
            "value": array[i - 1]
        }).appendTo($containerDiv);
    }
}

function generateAxis(id, array, $container) {
    var $containerDiv = $("<div/>").appendTo($container);
    for (let i = 1; i < 3; i++) {
        var label = $("<label/>", {
            "for": id + "-" + i
        }).append("A" + i).appendTo($containerDiv);
        var input = $("<input/>", {
            "id": id + "-" + i,
            "type": "text",
            "value": array[i - 1]
        }).appendTo($containerDiv);
    }

}

function drawSelect(id, array, container, selected = 0) {
    var select = $("<select/>", {
        "id": id
    });
    for (let i = 0; i < array.length; i++) {
        var option = $("<option/>", {
            "value": array[i],
            "selected": i == selected
        }).append(array[i]).appendTo(select);
    }
    container.append(select);
}

// Kopiranje kodova sa dna (na klik)

var copyToClipboard = function(textArea) {
    textArea.selectionStart = 0;
    textArea.selectionEnd = textArea.value.length;
    document.execCommand("copy");
    textArea.selectionEnd = 0; // deselektiramo
};

var copyJsonCode = function() {
    copyToClipboard(document.getElementById("jsonCode"));
};

var copyLispCode = function() {
    copyToClipboard(document.getElementById("lispCode"));
};

$(".generator").on("click", "#jsonCopy", copyJsonCode);
$(".generator").on("click", "#lispCopy", copyLispCode);
