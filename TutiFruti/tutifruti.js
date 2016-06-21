'use strict';

var fieldSize = 50; // in pixels
var fieldSizeName = "error";
var fieldMargin = 0; // in pixels

var messages = [];

var data1 = {
    board: {
        type: "hexagonal-pointy",
        dimensions: [3, 4, 5],
        corner: "bottom-left",
        axis: [
            "1 2 3 4 5 6 7 8 9 10 11 12 13 14 15",
            "A B C D E F G H I J K L M N"
        ],
        coloring: "chess",
        mode: "classic",
        size: "m"
    },
    player: {
        name: "La Plávusha",
        order: 1,
        message: "TutiFruti je awesome ♥"
    },
    state: [
        {
            fields: [
                ["1", "A"], ["2", "C"], ["2", "D"]
            ],
            style: {
                color: "black",
                shape: "circle"
            }
        },
        {
            fields: [
                ["2", "B"]
            ],
            style: {
                color: "white",
                shape: "circle"
            }
        }
    ],
    markings: [
        {
            fields: [
                ["3", "C"]
            ],
            style: {
                color: "pink",
                shape: "crosshairs"
            }
        }
    ],
    removed: [
        // Player 1's removed figures
        [
            {
                number: 1,
                style: {
                    color: "green",
                    shape: "chess-queen-fill"
                }
            },
            {
                number: 4,
                style: {
                    color: "blue",
                    shape: "chess-pawn-outline"
                }
            },
            {
                number: 2,
                style: {
                    color: "pink",
                    shape: "X"
                }
            }
        ],
        // Player 2's removed figures
        [
            {
                number: 1,
                style: {
                    color: "red",
                    shape: "suit-spade-fill"
                }
            },
            {
                number: 2,
                style: {
                    color: "teal",
                    shape: "circle-outline"
                }
            }
        ]
    ]
};

var displayData = function (data) {
    try {
        // Clear the board & messages first
        $(".board").html("");

        // Validate input
        validate(data);

        // Set global variables for sizes
        setSizes(data.board.size);

        // Display stuff
        displayDataBoard(data.board);
        displayDataState(data.state);
        displayDataPlayer(data.player);
        displayDataMarkings(data.markings);
        displayDataRemoved(data.removed);
    } catch (ex) {
        console.error(ex);
        displayError(ex);
    }
};


// --------------------------------------- //
// -------------  VALIDACIJA ------------- //
// --------------------------------------- //

var validate = function (data) {

    messages = [];
    displayMessages();

    // Validnost obaveznih grupa
    if (isNullOrUndefined(data)) {
        throw "Primljeni objekat `data` koji treba da sadrži sve podatke o trenutnom stanju igre " +
        "na osnovu koga se vrši prikaz ima vrednost `" + data + "`.";
    }

    if (isNullOrUndefined(data.board)) {
        throw "Nisu dobijeni podaci o tabli. " +
        "Objekat `board` ima vrednost `" + data.board + "`.";
    }

    if (isNullOrUndefined(data.player)) {
        throw "Nisu dobijeni podaci o tabli. " +
        "Objekat `board` ima vrednost `" + data.player + "`.";
    }

    // Validnost preporucenih grupa, i dodela podrazumevanih vrednosti
    if (isNullOrUndefined(data.state)) {
        displayWarning("Nije dobijena nijedna figura na tabli. " +
            "(Objekat `state` ima vrednost `" + data.state + "`).");
        data.state = [];
    }

    // Dopunjivanje podrazumevanim vrednostima neobaveznih grupa
    if (isNullOrUndefined(data.markings)) {
        data.markings = [];
    }

    if (isNullOrUndefined(data.removed) || data.removed.length === 0) {
        data.removed = [[], []];
    }

    /////
    // CHECKPOINT.
    // Sve grupe su instancirane na validnu vrednost
    /////

    // Type
    if (isNullOrUndefined(data.board.type)) {
        displayWarning("Nije definisana vrednost za `board.type`. " +
            "Postavlja se podrazumevana vrednost `rectangular`.");
        data.board.type = "rectangular";
    }
    if ($.inArray(data.board.type, _boardType) === -1) {
        throw "Dobijena vrednost za `board.type` je `" + data.board.type + "`. " +
        "\nPodržane vrednosti su `rectangular`, `hexagonal-flat` i `hexagonal-pointy`.";
    }

    // Dimensions
    if (isNullOrUndefined(data.board.dimensions)) {
        displayWarning("dimensions");
        if (isRectangular(data)) {
            data.board.dimensions = [8, 8];
        } else if (isHexagonal(data)) {
            data.board.dimensions = [6, 6, 6]
        } else {
            debugger;
            throw "Neočekivana greška.";
        }
    }
    if (isRectangular(data)) {
        if (data.board.dimensions.length < 2) {
            throw "Tabla je pravougaona, a broj dobijenih dimenzija je `" +
            data.board.dimensions.length + "`. Očekivani broj je `2`."
        } else if (data.board.dimensions.length > 2) {
            displayWarning("Tabla je pravougaona, a broj dobijenih dimenzija je `" +
                data.board.dimensions.length + "`. Očekivani broj je `2`. " +
                "Ignorišu se dimenzije koje su višak.");
            data.board.dimensions.splice(2); // ostaju prve dve
        }
    } else if (isHexagonal(data)) {
        if (data.board.dimensions.length < 3) {
            throw "Tabla je šestougaona (`" + data.board.type + "`), a broj dobijenih dimenzija je `" +
            data.board.dimensions.length + "`. Očekivani broj je `3`."
        } else if (data.board.dimensions.length > 3) {
            displayWarning("Tabla je šestougaona (`" + data.board.type + "`), a broj dobijenih dimenzija je `" +
                data.board.dimensions.length + "`. Očekivani broj je `3`. " +
                "Ignorišu se dimenzije koje su višak.");
            data.board.dimensions.splice(3); // ostaju prve tri
        }
    } else {
        debugger;
        throw "Neočekivana greška."
    }

    // Corner
    if (isNullOrUndefined(data.board.corner)) {
        displayWarning("corner not set");
        data.board.corner = "bottom-left";
    }
    if ($.inArray(data.board.corner, _boardCorners) === -1) {
        throw "Svojstvo `corner` je podešeno na `" + data.board.corner + "`. " +
        "Mora biti iz skupa vrednosti `" + _boardCorners.join("`, `") + "`.";
    }
    if (isRectangular(data) && $.inArray(data.board.corner, _boardCornersRectangular) === -1) {
        throw "Svojstvo `corner` je podešeno na `" + data.board.corner + "`. " +
        "Kod pravougaonih tabli, ova vrednost mora biti `" +
        _boardCornersRectangular.join("`, `") + "`.";
    }
    if (isHexagonalFlat(data) && $.inArray(data.board.corner, _boardCornersHexagonalFlat) === -1) {
        throw "Svojstvo `corner` je podešeno na `" + data.board.corner + "`. " +
        "Kod šestougaonih tabli sa stranom gore, ova vrednost mora biti `" +
        _boardCornersHexagonalFlat.join("`, `") + "`.";
    }
    if (isHexagonalPointy(data) && $.inArray(data.board.corner, _boardCornersHexagonalPointy) === -1) {
        throw "Svojstvo `corner` je podešeno na `" + data.board.corner + "`. " +
        "Kod šestougaonih tabli sa temenom gore, ova vrednost mora biti `" +
        _boardCornersHexagonalPointy.join("`, `") + "`.";
    }

    // Axis
    if (isNullOrUndefined(data.board.axis)) {
        displayWarning("axis not set");
        data.board.axis = [
            "1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26",
            "A B C D E F G H I J K L M N O P Q R S T U V W X Y Z"
        ];
    }
    // Duzine axisa
    var axisLength = [
        data.board.axis[0].split(' ').length,
        data.board.axis[1].split(' ').length
    ];
    // Parametri za prikaz poruke
    var justWarning = [], dimension = [], len = [];
    // Manje kucanja, da ne poludim
    var dims = data.board.dimensions;
    // Validacija duzine axisa
    if (isRectangular(data)) {
        for (let i = 0; i < 2; i++) {
            if (axisLength[i] < data.board.dimensions[i]) {
                justWarning.push(false);
                dimension.push(i);
                len.push(data.board.dimensions[i]);
            } else if (axisLength[i] > data.board.dimensions[i]) {
                justWarning.push(true);
                dimension.push(i);
                len.push(data.board.dimensions[i]);
            }
        }
    }
    if (isHexagonalFlat(data)) {
        let a = dims[0], b = dims[1], c = dims[2];
        let q = axisLength[0], w = axisLength[1];
        switch (data.board.corner) {
            case "bottom-left":
            case "top-right":
                if (q < a + c - 1) {
                    justWarning.push(false);
                    dimension.push(0);
                    len.push(a + c - 1);
                } else if (q > a + c - 1) {
                    justWarning.push(true);
                    dimension.push(0);
                    len.push(a + c - 1);
                }
                if (w < a + b - 1) {
                    justWarning.push(false);
                    dimension.push(1);
                    len.push(a + b - 1);
                } else if (w > a + b - 1) {
                    justWarning.push(true);
                    dimension.push(1);
                    len.push(a + b - 1);
                }
                break;
            case "bottom-right":
            case "top-left":
                if (q < b + c - 1) {
                    justWarning.push(false);
                    dimension.push(0);
                    len.push(b + c - 1);
                } else if (q > b + c - 1) {
                    justWarning.push(true);
                    dimension.push(0);
                    len.push(b + c - 1);
                }
                if (w < a + c - 1) {
                    justWarning.push(false);
                    dimension.push(1);
                    len.push(a + c - 1);
                } else if (w > a + c - 1) {
                    justWarning.push(true);
                    dimension.push(1);
                    len.push(a + c - 1);
                }
                break;
            case "right":
            case "left":
                if (q < b + a - 1) {
                    justWarning.push(false);
                    dimension.push(0);
                    len.push(b + a - 1);
                } else if (q > b + a - 1) {
                    justWarning.push(true);
                    dimension.push(0);
                    len.push(b + a - 1);
                }
                if (w < b + c - 1) {
                    justWarning.push(false);
                    dimension.push(1);
                    len.push(b + c - 1);
                } else if (w > b + c - 1) {
                    justWarning.push(true);
                    dimension.push(1);
                    len.push(b + c - 1);
                }
                break;
            default:
                debugger;
                throw "Neočekivana greška!";
        }
    }
    if (isHexagonalPointy(data)) {
        let a = dims[0], b = dims[1], c = dims[2];
        let q = axisLength[0], w = axisLength[1];
        switch (data.board.corner) {
            case "bottom-left":
            case "top-right":
                if (q < a + c - 1) {
                    justWarning.push(false);
                    dimension.push(0);
                    len.push(a + c - 1);
                } else if (q > a + c - 1) {
                    justWarning.push(true);
                    dimension.push(0);
                    len.push(a + c - 1);
                }
                if (w < a + b - 1) {
                    justWarning.push(false);
                    dimension.push(1);
                    len.push(a + b - 1);
                } else if (w > a + b - 1) {
                    justWarning.push(true);
                    dimension.push(1);
                    len.push(a + b - 1);
                }
                break;
            case "bottom-right":
            case "top-left":
                if (q < b + a - 1) {
                    justWarning.push(false);
                    dimension.push(0);
                    len.push(b + a - 1);
                } else if (q > b + a - 1) {
                    justWarning.push(true);
                    dimension.push(0);
                    len.push(b + a - 1);
                }
                if (w < b + c - 1) {
                    justWarning.push(false);
                    dimension.push(1);
                    len.push(b + c - 1);
                } else if (w > b + c - 1) {
                    justWarning.push(true);
                    dimension.push(1);
                    len.push(b + c - 1);
                }
                break;
            case "top":
            case "bottom":
                if (q < b + c - 1) {
                    justWarning.push(false);
                    dimension.push(0);
                    len.push(b + c - 1);
                } else if (q > b + c - 1) {
                    justWarning.push(true);
                    dimension.push(0);
                    len.push(b + c - 1);
                }
                if (w < a + c - 1) {
                    justWarning.push(false);
                    dimension.push(1);
                    len.push(a + c - 1);
                } else if (w > a + c - 1) {
                    justWarning.push(true);
                    dimension.push(1);
                    len.push(a + c - 1);
                }
                break;
            default:
                debugger;
                throw "Neočekivana greška!";
        }
    }
    // Rezultat prikupljanja podataka odozgo
    if (justWarning.length !== dimension.length) {
        debugger;
        throw "Neočekivana greška!"
    }
    for (var i = 0; i < justWarning.length; i++) {
        if (justWarning[i]) {
            displayWarning("Dobijeno je *" + axisLength[dimension[i]] + "* oznaka za " +
                (dimension[i] === 0 ? "prvu" : (dimension[i] === 1 ? "drugu" : "trecu")) + " dimenziju, a očekuje se " +
                "samo *" + len[i] + "*. Ignoriše se višak oznaka.");
            data.board.axis[dimension[i]] = data.board.axis[dimension[i]].split(' ').slice(0, len[i]).join(' ');
        } else {
            throw "Dobijeno je *" + axisLength[dimension[i]] + "* oznaka za " +
            (dimension[i] === 0 ? "prvu" : (dimension[i] === 1 ? "drugu" : "trecu")) + " dimenziju, a očekuje se *" + +len[i] + "*.";
        }
    }

    //  Coloring
    if (isNullOrUndefined(data.board.coloring)) {
        displayWarning("Dobijena vrednost svojstva `coloring` je `" + data.board.coloring + "`. " +
            "Očekivane vrednosti su `" + _boardColoring.join("`, `") + "`. " +
            "Postavlja se na podrazumevanu vrednost `classic`.");
        data.board.coloring = "classic";
    }
    if ($.inArray(data.board.coloring, _boardColoring) === -1) {
        throw "Dobijena vrednost svojstva `coloring` je `" + data.board.coloring + "`. " +
        "Očekivane vrednosti su `" + _boardColoring.join("`, `") + "`."
    }

    // Mode
    if (isNullOrUndefined(data.board.mode)) {
        displayWarning("Dobijena vrednost svojstva `mode` je `" + data.board.mode + "`. " +
            "Očekivane vrednosti su `" + _boardModeHexagonal.join("`, `") + "`. " +
            "Postavlja se na podrazumevanu vrednosti `classic`.");
        data.board.mode = "classic";
    }
    if (isHexagonal(data)) {
        if ($.inArray(data.board.mode, _boardModeHexagonal) === -1) {
            throw "Dobijena vrednost svojstva `mode` je `" + data.board.mode + "`. " +
            "Očekivane vrednosti su `" + _boardModeHexagonal.join("`, `") + "`."
        }
    } else if (isRectangular(data)) {
        if ($.inArray(data.board.mode, _boardModeRectangular) === -1) {
            throw "Dobijena vrednost svojstva `mode` je `" + data.board.mode + "`. " +
            "Očekivane vrednosti su `" + _boardModeRectangular.join("`, `") + "`."
        }
        // Do not allow combination coloring = chess / mode = go.
        if (data.board.coloring === "chess" && data.board.mode === "go") {
            throw "Nije dozvoljena kombinacija gde je svojstvo `coloring` postavljeno na `chess`," +
            " a `mode` na `go`.";
        }
    } else {
        debugger;
        throw "Neočekivana greška!";
    }

    //  Size
    if (isNullOrUndefined(data.board.size)) {
        displayWarning("Dobijena vrednost svojstva `size` je `" + data.board.size + "`. " +
            "Očekivane vrednosti su `" + _boardSize.join("`, `") + "`. " +
            "Postavlja se na porrazumevanu vrednost `m`.");
        data.board.size = "m";
    }
    if ($.inArray(data.board.size, _boardSize) === -1) {
        throw "Dobijena vrednost svojstva `size` je `" + data.board.size + "`. " +
        "Očekivane vrednosti su `" + _boardSize.join("`, `") + "`."
    }

    /////
    // CHECKPOINT.
    // Grupa 'board' je validna.
    /////

    // Order
    if (isNullOrUndefined(data.player.order)) {
        throw "Svojstvo `player.order` nije postavljeno. " +
        "Očekivane vrednosti: `1` ili `2`.";
    }
    if (+data.player.order !== 1 && +data.player.order !== 2) {
        throw "Svojstvo `player.order` je postavljeno na `" +
        data.player.order + "`. Očekivane vrednosti: `1` ili `2`."
    }

    // Name
    if (isNullOrUndefined(data.player.name)) {
        displayWarning("Svojstvo `player.name` nije postavljeno. " +
            "Koristi se podrazumevana vrednost `Igrač " + data.player.order + "`.");
        data.player.name = "Igrač " + data.player.order;
    }
    if (data.player.name === "") {
        displayWarning("Svojstvo `player.name` je podešeno na prazan string.");
    }

    // Message se ne ispituje

    /////
    // CHECKPOINT.
    // Grupa 'player' je validna.
    /////


    // State
    for (let i = 0; i < data.state.length; i++) {
        if (isNullOrUndefined(data.state[i].fields) && isNullOrUndefined(data.state[i].style)) {
            displayWarning("Element `state[" + i + "]` nema postavljeno ni `fields` ni `style`.");
        } else if (!isNullOrUndefined(data.state[i].style) &&
            (isNullOrUndefined(data.state[i].style.color) || isNullOrUndefined(data.state[i].style.shape))) {
            displayWarning("Element `state[" + i + "].style` nema postavljena svojstva `color` i `shape`.");
        } else if (isNullOrUndefined(data.state[i].fields)) {
            displayWarning("Dobijen objekat `state[" + i + "]` ima postavljen stil na " +
                "`" + data.state[i].style.color + "` i `" + data.state[i].style.shape + "`, ali nema `fields`.");
        } else if ($.inArray(data.state[i].style.color, _styleColor) === -1) {
            displayWarning("Dobijen objekat `state[" + i + "]` koji treba smestiti na polja `" +
                data.state[i].fields.map(i => i.join('-')).join(', ') + "` ima kao boju navedeno " +
                "`" + data.state[i].style.color + "`, što nije validna boja. Spisak boja pogledati u dokumentaciji.");
        } else if ($.inArray(data.state[i].style.shape, _stateShape) === -1) {
            displayWarning("Dobijen objekat `state[" + i + "]` koji treba smestiti na polja `" +
                data.state[i].fields.map(i => i.join('-')).join(', ') + "` ima kao oblik navedeno " +
                "`" + data.state[i].style.shape + "`, što nije validan oblik. Spisak oblika pogledati u dokumentaciji.");
        }
    }


    // Markings
    for (let i = 0; i < data.markings.length; i++) {
        if (isNullOrUndefined(data.markings[i].fields) && isNullOrUndefined(data.markings[i].style)) {
            displayWarning("Element `markings[" + i + "]` nema postavljeno ni `fields` ni `style`.");
        } else if (!isNullOrUndefined(data.markings[i].style) &&
            (isNullOrUndefined(data.markings[i].style.color) || isNullOrUndefined(data.markings[i].style.shape))) {
            displayWarning("Element `markings[" + i + "].style` nema postavljena svojstva `color` i `shape`.");
        } else if (isNullOrUndefined(data.markings[i].fields)) {
            displayWarning("Dobijen objekat `markings[" + i + "]` ima postavljen stil na " +
                "`" + data.markings[i].style.color + "` i `" + data.markings[i].style.shape + "`, ali nema `fields`.");
        } else if ($.inArray(data.markings[i].style.color, _styleColor) === -1) {
            displayWarning("Dobijen objekat `markings[" + i + "]` koji treba smestiti na polja `" +
                data.markings[i].fields.map(i => i.join('-')).join(', ') + "` ima kao boju navedeno " +
                "`" + data.markings[i].style.color + "`, što nije validna boja. Spisak boja pogledati u dokumentaciji.");
        } else if ($.inArray(data.markings[i].style.shape, _stateShape) === -1) {
            displayWarning("Dobijen objekat `markings[" + i + "]` koji treba smestiti na polja `" +
                data.markings[i].fields.map(i => i.join('-')).join(', ') + "` ima kao oblik navedeno " +
                "`" + data.markings[i].style.shape + "`, što nije validan oblik. Spisak oblika pogledati u dokumentaciji.");
        }
    }

    // Removed
    for (let i = 0; i < data.removed.length; i++) {
        for (let j = 0; j < 2; j++) {
            if (data.removed[j] == null || data.removed[j].length === 0) continue;
            if (isNullOrUndefined(data.removed[j][i].number) && isNullOrUndefined(data.removed[j][i].style)) {
                displayWarning("Element `removed[" + j + "][" + i + "]` nema postavljeno ni `number` ni `style`.");
            } else if (!isNullOrUndefined(data.removed[j][i].style) &&
                (isNullOrUndefined(data.removed[j][i].style.color) || isNullOrUndefined(data.removed[j][i].style.shape))) {
                displayWarning("Element `removed[" + j + "][" + i + "].style` nema postavljena svojstva `color` i `shape`.");
            } else if (isNullOrUndefined(data.removed[j][i].number)) {
                displayWarning("Dobijen objekat `removed[" + j + "][" + i + "]` ima postavljen stil na " +
                    "`" + data.removed[j][i].style.color + "` i `" + data.removed[j][i].style.shape + "`, ali nema `fields`.");
            } else if ($.inArray(data.removed[j][i].style.color, _styleColor) === -1) {
                displayWarning("Dobijen objekat `removed[" + j + "][" + i + "]` kojih ima `" +
                    data.removed[j][i].number + "` ima kao boju navedeno " +
                    "`" + data.removed[j][i].style.color + "`, što nije validna boja. Spisak boja pogledati u dokumentaciji.");
            } else if ($.inArray(data.removed[j][i].style.shape, _stateShape) === -1) {
                displayWarning("Dobijen objekat `removed[" + j + "][" + i + "]` kojih ima `" +
                    data.removed[j][i].number + "` ima kao oblik navedeno " +
                    "`" + data.removed[j][i].style.shape + "`, što nije validan oblik. Spisak oblika pogledati u dokumentaciji.");
            }
        }
    }

    displayMessages();
};

var isNullOrUndefined = function (stuff) {
    return stuff === null || stuff == undefined;
};

var isRectangular = function (data) {
    return data.board.type === "rectangular";
};

var isHexagonalPointy = function (data) {
    return data.board.type === "hexagonal-pointy";
};

var isHexagonalFlat = function (data) {
    return data.board.type === "hexagonal-flat";
};

var isHexagonal = function (data) {
    return data.board.type === "hexagonal-flat" || data.board.type === "hexagonal-pointy";
};

const _boardType = ["rectangular", "hexagonal-flat", "hexagonal-pointy"];

const _boardCorners = ["bottom-left", "top-left", "top-right", "bottom-right", "left", "right", "bottom", "top"];
const _boardCornersRectangular = ["bottom-left", "top-left", "bottom-right", "top-right"];
const _boardCornersHexagonalFlat = ["bottom-left", "top-left", "top-right", "bottom-right", "left", "right"];
const _boardCornersHexagonalPointy = ["bottom-left", "top-left", "top-right", "bottom-right", "top", "bottom"];

const _boardColoring = ["classic", "chess"];

const _boardModeHexagonal = ["classic", "circles"];
var temp = _boardModeHexagonal;
temp.push("go");
const _boardModeRectangular = temp;

const _boardSize = ["xxs", "xs", "s", "m", "l", "xl", "xxl"];

const _styleColor = ["red", "pink", "purple", "deep-purple", "indigo", "blue",
    "light-blue", "cyan", "teal", "green", "light-green", "lime", "yellow", "amber",
    "deep-orange", "brown", "grey", "blue-grey", "black", "white"].map(i => i.trim());
var ___stateShape = ["O", "X", "circle", "square", "star", "circle-outline", "square-outline",
    "arrow-left", "arrow-right", "arrow-up", "arrow-down",
    "angle-left", "angle-right", "angle-up", "angle-down",
    "angle-double-left", "angle-double-right", "angle-double-up", "angle-double-down",
    "crosshairs", "bullseye", "check", "check-circle", "minus", "plus",
    "suit-spade-fill", "suit-spade-outline", "suit-heart-fill", "suit-heart-outline",
    "suit-diamond-fill", "suit-diamond-outline", "suit-club-fill", "suit-club-outline",
    "chess-king-outline", "chess-king-fill", "chess-queen-outline", "chess-queen-fill",
    "chess-rook-outline", "chess-rook-fill", "chess-bishop-outline", "chess-bishop-fill",
    "chess-knight-outline", "chess-knight-fill", "chess-pawn-outline", "chess-pawn-fill"].map(i => i.trim());
const _stateShape = ___stateShape
    .concat((new Array(99)).fill(1).map((e, i) => e + i).map(String).map(i => "number-" + i))
    .concat((new Array(26)).fill(0x41).map((e, i) => e + i).map(i => String.fromCharCode(i)).map(i => "letter-" + i))
    .concat((new Array(26)).fill(0x61).map((e, i) => e + i).map(i => String.fromCharCode(i)).map(i => "letter-" + i));


// --------------------------------------- //
// -------------  DATA BOARD ------------- //
// --------------------------------------- //

var setSizes = function (size) {
    switch (size) {
        case "xxs":
            fieldSize = 26;
            break;
        case "xs":
            fieldSize = 36;
            break;
        case "s":
            fieldSize = 42;
            break;
        case "m":
            fieldSize = 48;
            break;
        case "l":
            fieldSize = 56;
            break;
        case "xl":
            fieldSize = 64;
            break;
        case "xxl":
            fieldSize = 72;
            break;
        default:
            throw "Nevalidna vrednost veličine. " +
            "Očekuje se xxs, xs, s, m, l, xl ili xxl, " +
            "a dobijeno je " + size + ".";
            break;
    }
    fieldSizeName = size;
};

var displayDataBoard = function (dataBoard) {
    switch (dataBoard.type) {
        case "rectangular":
            if (dataBoard.dimensions.length < 2)
                throw ("Tabla je pravougaona, a broj dobijenih dimenzija je " +
                dataBoard.dimensions.length + ". Treba da bude 2.");
            if (dataBoard.dimensions.length > 2)
                displayWarning("Tabla je pravougaona, a broj dobijenih dimenzija je " +
                    dataBoard.dimensions.length + ". Koristim samo prve 2 dimenzije, ostale ignorišem.");
            drawRectangularBoard(dataBoard.dimensions[0], dataBoard.dimensions[1]);
            break;
        case "hexagonal-flat":
            if (dataBoard.dimensions.length < 3)
                throw ("Tabla je šestougaona, a broj dobijenih dimenzija je " +
                dataBoard.dimensions.length + ". Treba da bude 3.");
            if (dataBoard.dimensions.length > 3)
                displayWarning("Tabla je šestougaona, a broj dobijenih dimenzija je " +
                    dataBoard.dimensions.length + ". Koristim samo prve 3 dimenzije, ostale ignorišem.");
            drawHexagonalFlatBoard(dataBoard.dimensions[0], dataBoard.dimensions[1], dataBoard.dimensions[2]);
            break;
        case "hexagonal-pointy":
            if (dataBoard.dimensions.length < 3)
                throw ("Tabla je šestougaona, a broj dobijenih dimenzija je " +
                dataBoard.dimensions.length + ". Treba da bude 3.");
            if (dataBoard.dimensions.length > 3)
                displayWarning("Tabla je šestougaona, a broj dobijenih dimenzija je " +
                    dataBoard.dimensions.length + ". Koristim samo prve 3 dimenzije, ostale ignorišem.");
            drawHexagonalPointyBoard(dataBoard.dimensions[0], dataBoard.dimensions[1], dataBoard.dimensions[2]);
            break;
        default:
            throw "Dobijen tip table je " + dataBoard.type + ". Tip table mora biti rectangular, triangular-pointy, triangular-flat, hexagonal-pointy ili hexagonal-flat."
    }
    $('.board').attr('data-board-type', dataBoard.type);
    drawAxis(dataBoard.type, dataBoard.dimensions, dataBoard.axis, dataBoard.corner);
    switch (dataBoard.coloring) {
        case "chess":
            drawChess();
            break;
        case "classic":
            break;
        default:
            displayWarning("Tip bojenja nije ni classic ni chess. Ignoriše se.");
    }
    switch (dataBoard.mode) {
        case "go":
            drawGoStyle();
            break;
        case "circles":
            drawCircleStyle();
            break;
        case "classic":
            break;
        default:
            displayWarning("Dobijeni mod table je " + dataBoard.mode +
                ". Ocečekivane vrednosti su: classic, circles, go. " +
                "Mod se ignoriše i uzima se classic.");
    }
};

var drawRectangularBoard = function (rows, cols) {
    if (rows <= 0 || Math.ceil(rows) !== rows)
        throw "Dobijen broj redova je " + rows + ". Broj redova mora biti nenegativan ceo broj.";
    if (cols <= 0 || Math.ceil(cols) !== cols)
        throw "Dobijen broj redova je " + cols + ". Broj redova mora biti nenegativan ceo broj.";
    var $board = $('.board');
    var boardSize = rows * cols; // number of fields
    var totalFieldSize = fieldSize + 2 * fieldMargin;
    for (var i = 0; i < boardSize; i++) {
        var x = Math.floor(i / cols);
        var y = i % cols;
        var posX = (x - rows / 2) * totalFieldSize;
        var posY = (y - cols / 2) * totalFieldSize;
        $('<div/>', {
            class: 'field rectangle size-' + fieldSizeName,
            "data-coordinates": '' + x + '-' + y,
            style: 'left: ' + posY + 'px; top: ' + posX + 'px'
        })
            .append('<span>' + x + '-' + y + '</span>')
            .appendTo($board);
    }
};

// Throws if something is wrong.
// Used by both types of hexagonal boards.
var checkHexagonalDimensions = function (a, b, c) {
    if (a <= 0 || Math.ceil(a) !== a) {
        throw "Dobijene dimenzije su [" + a + ", " + ", " + b + ", " + c + "]. " +
        "Prva dimenzija (" + a + ") je neispravna vrednost. " +
        "Svaka dimenzija mora biti nenegativan ceo broj."
    }
    if (b <= 0 || Math.ceil(b) !== b) {
        throw "Dobijene dimenzije su [" + a + ", " + ", " + b + ", " + c + "]. " +
        "Druga dimenzija (" + a + ") je neispravna vrednost. " +
        "Svaka dimenzija mora biti nenegativan ceo broj."
    }
    if (c <= 0 || Math.ceil(c) !== c) {
        throw "Dobijene dimenzije su [" + a + ", " + ", " + b + ", " + c + "]. " +
        "Treća dimenzija (" + a + ") je neispravna vrednost. " +
        "Svaka dimenzija mora biti nenegativan ceo broj."
    }
};

var hexagonalFlatCoordinatesToPosition = function (x, y, size, dimension) {
    size = size / Math.sqrt(3) / 2;
    var cubeZ = x;
    var cubeY = -y;
    var cubeX = -(cubeY + cubeZ);
    cubeX -= (dimension.a + dimension.b - 1) / 2 - dimension.a + 1;
    cubeZ -= (dimension.c + dimension.a - 1) / 2;
    var posX = 2 * size * Math.sqrt(3) * (cubeX + cubeZ / 2);
    var posY = 2 * size * (3 / 2) * cubeZ;
    posY += size / 2;
    posX += size * Math.sqrt(3) / 2;
    return [posX, posY];
};

var drawHexagonalFlatBoard = function (a, b, c) {
    checkHexagonalDimensions(a, b, c);
    var $board = $('.board');
    var totalFieldSize = fieldSize + 2 * fieldMargin;
    var size = {a: a, b: b, c: c};
    var pos, i, j;
    // top part
    var w = b;
    for (i = 0; i < a; i++) {
        for (j = 0; j < w; j++) {
            pos = hexagonalFlatCoordinatesToPosition(i, j, totalFieldSize, size);
            $('<div/>', {
                class: 'field hexagon-pointy size-' + fieldSizeName,
                "data-coordinates": '' + i + '-' + j,
                style: 'left: ' + pos[0] + 'px; top: ' + pos[1] + 'px;'
            })
                .append('<span>' + i + '-' + j + '</span>')
                .appendTo($board);
        }
        w++; // increase the width
    }
    // middle part
    var o = 1;
    for (i = 0 + a; i < c; i++) {
        for (j = o; j < w + o - 1; j++) {
            pos = hexagonalFlatCoordinatesToPosition(i, j, totalFieldSize, size);
            $('<div/>', {
                class: 'field hexagon-pointy size-' + fieldSizeName,
                "data-coordinates": '' + i + '-' + j,
                style: 'left: ' + pos[0] + 'px; top: ' + pos[1] + 'px;'
            })
                .append('<span>' + i + '-' + j + '</span>')
                .appendTo($board);
        }
        o++; // increase offset
    }
    // bottom part
    for (i = c; i < c + a - 1; i++) {
        w--; // decrease width
        for (j = o; j < o + w - 1; j++) {
            pos = hexagonalFlatCoordinatesToPosition(i, j, totalFieldSize, size);
            $('<div/>', {
                class: 'field hexagon-pointy size-' + fieldSizeName,
                "data-coordinates": '' + i + '-' + j,
                style: 'left: ' + pos[0] + 'px; top: ' + pos[1] + 'px;'
            })
                .append('<span>' + i + '-' + j + '</span>')
                .appendTo($board);
        }
        o++; // increase offset
    }
};

var hexagonalPointyCoordinatesToPosition = function (x, y, size, dimension) {
    size = size / Math.sqrt(3) / 2;
    var cubeZ = x;
    var cubeY = -y;
    var cubeX = -(cubeY + cubeZ);
    cubeX -= (dimension.a + dimension.b - 1) / 2 - dimension.a + 1;
    cubeZ -= (dimension.c + dimension.a - 1) / 2;
    var posY = 2 * size * Math.sqrt(3) * (cubeZ + cubeX / 2);
    var posX = 2 * size * (3 / 2) * cubeX;
    posY += size / 2;
    posX += size * Math.sqrt(3) / 2;
    return [posX, posY];
};

var drawHexagonalPointyBoard = function (a, b, c) {
    checkHexagonalDimensions(a, b, c);
    var $board = $('.board');
    var totalFieldSize = fieldSize + 2 * fieldMargin;
    var size = {a: a, b: b, c: c};
    var pos, i, j;
    // top part
    var w = b;
    for (i = 0; i < a; i++) {
        for (j = 0; j < w; j++) {
            pos = hexagonalPointyCoordinatesToPosition(i, j, totalFieldSize, size);
            $('<div/>', {
                class: 'field hexagon-flat size-' + fieldSizeName,
                "data-coordinates": '' + i + '-' + j,
                style: 'left: ' + pos[0] + 'px; top: ' + pos[1] + 'px;'
            })
                .append('<span>' + i + '-' + j + '</span>')
                .appendTo($board);
        }
        w++; // increase the width
    }
    // middle part
    var o = 1;
    for (i = 0 + a; i < c; i++) {
        for (j = o; j < w + o - 1; j++) {
            pos = hexagonalPointyCoordinatesToPosition(i, j, totalFieldSize, size);
            $('<div/>', {
                class: 'field hexagon-flat size-' + fieldSizeName,
                "data-coordinates": '' + i + '-' + j,
                style: 'left: ' + pos[0] + 'px; top: ' + pos[1] + 'px;'
            })
                .append('<span>' + i + '-' + j + '</span>')
                .appendTo($board);
        }
        o++; // increase offset
    }
    // bottom part
    for (i = c; i < c + a - 1; i++) {
        w--; // decrease width
        for (j = o; j < o + w - 1; j++) {
            pos = hexagonalPointyCoordinatesToPosition(i, j, totalFieldSize, size);
            $('<div/>', {
                class: 'field hexagon-flat size-' + fieldSizeName,
                "data-coordinates": '' + i + '-' + j,
                style: 'left: ' + pos[0] + 'px; top: ' + pos[1] + 'px;'
            })
                .append('<span>' + i + '-' + j + '</span>')
                .appendTo($board);
        }
        o++; // increase offset
    }
};

var drawAxis = function (type, dimensions, axis, corner) {
    var i = 0;
    if (axis.length !== 2) {
        throw "Očekuju se tačno dve liste simbola za označavanje."
    }
    var $board = $('.board');
    axis[0] = axis[0].split(' ');
    axis[1] = axis[1].split(' ');
    let firstHalf, secondHalf;
    switch (type) {
        case "rectangular":
            for (i = 0; i < 2; i++)
                if (axis[i].length !== dimensions[i]) throw "Loš broj oznaka za " +
                (i == 0 ? "prvu" : "drugu") + " osu. Očekuje se " +
                dimensions[i] + ", a dobijeno je " + axis[i].length + ".";
            var rows = dimensions[0];
            var cols = dimensions[1];
            // Sledi magija.
            // https://www.youtube.com/watch?v=1DFZtrbrf2Y
            var cornerOffsetXRows = 1;
            var cornerOffsetYRows = 1;
            var cornerMirrorXRows = -1;
            var cornerMirrorYRows = -1;
            var cornerOffsetXCols = 0;
            var cornerOffsetYCols = 0;
            var cornerMirrorXCols = 1;
            var cornerMirrorYCols = 1;
            switch (corner) {
                case "bottom-left":
                    cornerOffsetXRows = 1;
                    cornerOffsetYRows = 1;
                    cornerMirrorXRows = -1;
                    cornerMirrorYRows = -1;
                    cornerOffsetXCols = 0;
                    cornerOffsetYCols = 0;
                    cornerMirrorXCols = 1;
                    cornerMirrorYCols = 1;
                    break;
                case "bottom-right":
                    cornerOffsetXRows = 0;
                    cornerOffsetYRows = 0;
                    cornerMirrorXRows = 1;
                    cornerMirrorYRows = 1;
                    cornerOffsetXCols = 0;
                    cornerOffsetYCols = 0;
                    cornerMirrorXCols = 1;
                    cornerMirrorYCols = 1;
                    axis[0] = axis[0].reverse();
                    axis[1] = axis[1].reverse();
                    break;
                case "top-left":
                    cornerOffsetXRows = 1;
                    cornerOffsetYRows = 1;
                    cornerMirrorXRows = -1;
                    cornerMirrorYRows = -1;
                    cornerOffsetXCols = 1;
                    cornerOffsetYCols = 1;
                    cornerMirrorXCols = -1;
                    cornerMirrorYCols = -1;
                    axis[0] = axis[0].reverse();
                    axis[1] = axis[1].reverse();
                    break;
                case "top-right":
                    cornerOffsetXRows = 0;
                    cornerOffsetYRows = 0;
                    cornerMirrorXRows = 1;
                    cornerMirrorYRows = 1;
                    cornerOffsetXCols = 1;
                    cornerOffsetYCols = 1;
                    cornerMirrorXCols = -1;
                    cornerMirrorYCols = -1;
                    break;
                default:
                    displayWarning("Dobijeno " + corner + " za početni ugao. Očekuje se bottom-left, bottom-right, top-left ili top-right. Ignoriše se.");
            }
            var totalFieldSize = fieldSize + 2 * fieldMargin;
            var j = 0;
            var posX, posY;
            for (j = 0; j < cols; j++) {
                posX = cornerMirrorXCols * (j - cols / 2) * totalFieldSize - totalFieldSize * cornerOffsetXCols;
                posY = cornerMirrorYCols * rows / 2 * totalFieldSize - totalFieldSize * cornerOffsetYCols;
                $board.html($board.html() + '<div class="axis-field" style="' +
                    'left: ' + posX + 'px; ' +
                    'top: ' + posY + 'px; ' +
                    'height: ' + fieldSize + 'px; ' +
                    'width: ' + fieldSize + 'px; ' +
                    '"><span>' + axis[1][j] + '</span></div>')
            }
            for (j = 0; j < rows; j++) {
                posY = cornerMirrorYRows * (j - rows / 2) * totalFieldSize - totalFieldSize * cornerOffsetXRows;
                posX = cornerMirrorXRows * (cols) / 2 * totalFieldSize - totalFieldSize * cornerOffsetXRows;
                $board.html($board.html() + '<div class="axis-field" style="' +
                    'left: ' + posX + 'px; ' +
                    'top: ' + posY + 'px; ' +
                    'height: ' + fieldSize + 'px; ' +
                    'width: ' + fieldSize + 'px; ' +
                    '"><span>' + axis[0][j] + '</span></div>')
            }
            break;
        case "hexagonal-flat":
            switch (corner) {
                case "top-left":
                    // clockwise
                    firstHalf = axis[0].slice(0, dimensions[1]);
                    secondHalf = axis[0].slice(dimensions[1], dimensions[1] + dimensions[2] - 1);
                    firstHalf.unshift(" ");
                    secondHalf.push(" ");
                    secondHalf.unshift(" ");
                    displayAxisHexagonalFlatTop(firstHalf, dimensions);
                    displayAxisHexagonalFlatTopRight(secondHalf, dimensions);
                    // counter-clockwise
                    firstHalf = axis[1].slice(0, dimensions[0]);
                    secondHalf = axis[1].slice(dimensions[0], dimensions[0] + dimensions[2] - 1);
                    firstHalf.unshift(" ");
                    secondHalf.push(" ");
                    secondHalf.unshift(" ");
                    displayAxisHexagonalFlatTopLeft(firstHalf, dimensions);
                    displayAxisHexagonalFlatBottomLeft(secondHalf, dimensions);
                    break;
                case "bottom-left":
                    // clockwise
                    firstHalf = axis[0].slice(0, dimensions[2]);
                    secondHalf = axis[0].slice(dimensions[2], dimensions[2] + dimensions[0] - 1);
                    firstHalf.unshift(" ");
                    secondHalf.push(" ");
                    secondHalf.unshift(" ");
                    displayAxisHexagonalFlatBottomLeft(firstHalf.reverse(), dimensions);
                    displayAxisHexagonalFlatTopLeft(secondHalf.reverse(), dimensions);
                    // counter-clockwise
                    firstHalf = axis[1].slice(0, dimensions[1]);
                    secondHalf = axis[1].slice(dimensions[1], dimensions[1] + dimensions[0] - 1);
                    firstHalf.unshift(" ");
                    secondHalf.push(" ");
                    secondHalf.unshift(" ");
                    displayAxisHexagonalFlatBottom(firstHalf, dimensions);
                    displayAxisHexagonalFlatBottomRight(secondHalf, dimensions);
                    break;
                case "top-right":
                    // clockwise
                    firstHalf = axis[0].slice(0, dimensions[2]);
                    secondHalf = axis[0].slice(dimensions[2], dimensions[2] + dimensions[0] - 1);
                    firstHalf.unshift(" ");
                    secondHalf.push(" ");
                    secondHalf.unshift(" ");
                    displayAxisHexagonalFlatTopRight(firstHalf, dimensions);
                    displayAxisHexagonalFlatBottomRight(secondHalf.reverse(), dimensions);
                    // counter-clockwise
                    firstHalf = axis[1].slice(0, dimensions[1]);
                    secondHalf = axis[1].slice(dimensions[1], dimensions[1] + dimensions[0] - 1);
                    firstHalf.unshift(" ");
                    secondHalf.push(" ");
                    secondHalf.unshift(" ");
                    displayAxisHexagonalFlatTop(firstHalf.reverse(), dimensions);
                    displayAxisHexagonalFlatTopLeft(secondHalf, dimensions);
                    break;
                case "bottom-right":
                    // clockwise
                    firstHalf = axis[0].slice(0, dimensions[1]);
                    secondHalf = axis[0].slice(dimensions[1], dimensions[1] + dimensions[2] - 1);
                    firstHalf.unshift(" ");
                    secondHalf.push(" ");
                    secondHalf.unshift(" ");
                    displayAxisHexagonalFlatBottom(firstHalf.reverse(), dimensions);
                    displayAxisHexagonalFlatBottomLeft(secondHalf.reverse(), dimensions);
                    // counter-clockwise
                    firstHalf = axis[1].slice(0, dimensions[0]);
                    secondHalf = axis[1].slice(dimensions[0], dimensions[0] + dimensions[2] - 1);
                    firstHalf.unshift(" ");
                    secondHalf.push(" ");
                    secondHalf.unshift(" ");
                    displayAxisHexagonalFlatBottomRight(firstHalf, dimensions);
                    displayAxisHexagonalFlatTopRight(secondHalf.reverse(), dimensions);
                    break;
                case "right":
                    // clockwise
                    firstHalf = axis[0].slice(0, dimensions[0]);
                    secondHalf = axis[0].slice(dimensions[0], dimensions[0] + dimensions[1] - 1);
                    firstHalf.unshift(" ");
                    secondHalf.push(" ");
                    secondHalf.unshift(" ");
                    displayAxisHexagonalFlatBottomRight(firstHalf.reverse(), dimensions);
                    displayAxisHexagonalFlatBottom(secondHalf.reverse(), dimensions);
                    // counter-clockwise
                    firstHalf = axis[1].slice(0, dimensions[2]);
                    secondHalf = axis[1].slice(dimensions[2], dimensions[2] + dimensions[1] - 1);
                    firstHalf.unshift(" ");
                    secondHalf.push(" ");
                    secondHalf.unshift(" ");
                    displayAxisHexagonalFlatTopRight(firstHalf.reverse(), dimensions);
                    displayAxisHexagonalFlatTop(secondHalf.reverse(), dimensions);
                    break;
                case "left":
                    // clockwise
                    firstHalf = axis[0].slice(0, dimensions[0]);
                    secondHalf = axis[0].slice(dimensions[0], dimensions[0] + dimensions[1] - 1);
                    firstHalf.unshift(" ");
                    secondHalf.push(" ");
                    secondHalf.unshift(" ");
                    displayAxisHexagonalFlatTopLeft(firstHalf.reverse(), dimensions);
                    displayAxisHexagonalFlatTop(secondHalf, dimensions);
                    // counter-clockwise
                    firstHalf = axis[1].slice(0, dimensions[2]);
                    secondHalf = axis[1].slice(dimensions[2], dimensions[2] + dimensions[1] - 1);
                    firstHalf.unshift(" ");
                    secondHalf.push(" ");
                    secondHalf.unshift(" ");
                    displayAxisHexagonalFlatBottomLeft(firstHalf, dimensions);
                    displayAxisHexagonalFlatBottom(secondHalf, dimensions);
                    break;
                default:
                    throw "Neočekivana greška.";
            }
            break;
        case "hexagonal-pointy":
            switch (corner) {
                case "bottom":
                    // clockwise
                    firstHalf = axis[0].slice(0, dimensions[1]);
                    secondHalf = axis[0].slice(dimensions[1], dimensions[1] + dimensions[2] - 1);
                    firstHalf.unshift(" ");
                    secondHalf.push(" ");
                    secondHalf.unshift(" ");
                    displayAxisHexagonalPointyBottomLeft(firstHalf.reverse(), dimensions);
                    displayAxisHexagonalPointyLeft(secondHalf.reverse(), dimensions);
                    // counter-clockwise
                    firstHalf = axis[1].slice(0, dimensions[0]);
                    secondHalf = axis[1].slice(dimensions[0], dimensions[0] + dimensions[2] - 1);
                    firstHalf.unshift(" ");
                    secondHalf.push(" ");
                    secondHalf.unshift(" ");
                    displayAxisHexagonalPointyBottomRight(firstHalf, dimensions);
                    displayAxisHexagonalPointyRight(secondHalf.reverse(), dimensions);
                    break;
                case "bottom-right":
                    // clockwise
                    firstHalf = axis[0].slice(0, dimensions[0]);
                    secondHalf = axis[0].slice(dimensions[0], dimensions[0] + dimensions[1] - 1);
                    firstHalf.unshift("");
                    secondHalf.unshift(" ");
                    secondHalf.push(" ");
                    displayAxisHexagonalPointyBottomRight(firstHalf.reverse(), dimensions);
                    displayAxisHexagonalPointyBottomLeft(secondHalf.reverse(), dimensions);
                    // counter-clockwise
                    firstHalf = axis[1].slice(0, dimensions[2]);
                    secondHalf = axis[1].slice(dimensions[2], dimensions[2] + dimensions[1] - 1);
                    firstHalf.unshift("");
                    secondHalf.unshift(" ");
                    secondHalf.push(" ");
                    displayAxisHexagonalPointyRight(firstHalf.reverse(), dimensions);
                    displayAxisHexagonalPointyTopRight(secondHalf.reverse(), dimensions);
                    break;
                case "bottom-left":
                    // clockwise
                    firstHalf = axis[0].slice(0, dimensions[2]);
                    secondHalf = axis[0].slice(dimensions[2], dimensions[2] + dimensions[0] - 1);
                    firstHalf.unshift("");
                    secondHalf.unshift(" ");
                    secondHalf.push(" ");
                    displayAxisHexagonalPointyLeft(firstHalf.reverse(), dimensions);
                    displayAxisHexagonalPointyTopLeft(secondHalf.reverse(), dimensions);
                    // counter-clockwise
                    firstHalf = axis[1].slice(0, dimensions[1]);
                    secondHalf = axis[1].slice(dimensions[1], dimensions[1] + dimensions[0] - 1);
                    firstHalf.unshift("");
                    secondHalf.unshift(" ");
                    secondHalf.push(" ");
                    displayAxisHexagonalPointyBottomLeft(firstHalf, dimensions);
                    displayAxisHexagonalPointyBottomRight(secondHalf, dimensions);
                    break;
                case "top-left":
                    // clockwise
                    firstHalf = axis[0].slice(0, dimensions[0]);
                    secondHalf = axis[0].slice(dimensions[0], dimensions[0] + dimensions[1] - 1);
                    firstHalf.unshift("");
                    secondHalf.unshift(" ");
                    secondHalf.push(" ");
                    displayAxisHexagonalPointyTopLeft(firstHalf.reverse(), dimensions);
                    displayAxisHexagonalPointyTopRight(secondHalf, dimensions);
                    // counter-clockwise
                    firstHalf = axis[1].slice(0, dimensions[2]);
                    secondHalf = axis[1].slice(dimensions[2], dimensions[2] + dimensions[1] - 1);
                    firstHalf.unshift("");
                    secondHalf.unshift(" ");
                    secondHalf.push(" ");
                    displayAxisHexagonalPointyLeft(firstHalf, dimensions);
                    displayAxisHexagonalPointyBottomLeft(secondHalf, dimensions);
                    break;
                case "top-right":
                    // clockwise
                    firstHalf = axis[0].slice(0, dimensions[2]);
                    secondHalf = axis[0].slice(dimensions[2], dimensions[2] + dimensions[0] - 1);
                    firstHalf.unshift("");
                    secondHalf.unshift(" ");
                    secondHalf.push(" ");
                    displayAxisHexagonalPointyRight(firstHalf, dimensions);
                    displayAxisHexagonalPointyBottomRight(secondHalf.reverse(), dimensions);
                    // counter-clockwise
                    firstHalf = axis[1].slice(0, dimensions[1]);
                    secondHalf = axis[1].slice(dimensions[1], dimensions[1] + dimensions[0] - 1);
                    firstHalf.unshift("");
                    secondHalf.unshift(" ");
                    secondHalf.push(" ");
                    displayAxisHexagonalPointyTopRight(firstHalf.reverse(), dimensions);
                    displayAxisHexagonalPointyTopLeft(secondHalf, dimensions);
                    break;
                case "top":
                    // clockwise
                    firstHalf = axis[0].slice(0, dimensions[1]);
                    secondHalf = axis[0].slice(dimensions[1], dimensions[1] + dimensions[2] - 1);
                    firstHalf.unshift("");
                    secondHalf.unshift(" ");
                    secondHalf.push(" ");
                    displayAxisHexagonalPointyTopRight(firstHalf, dimensions);
                    displayAxisHexagonalPointyRight(secondHalf, dimensions);
                    // counter-clockwise
                    firstHalf = axis[1].slice(0, dimensions[0]);
                    secondHalf = axis[1].slice(dimensions[0], dimensions[0] + dimensions[2] - 1);
                    firstHalf.unshift("");
                    secondHalf.unshift(" ");
                    secondHalf.push(" ");
                    displayAxisHexagonalPointyTopLeft(firstHalf, dimensions);
                    displayAxisHexagonalPointyLeft(secondHalf, dimensions);
                    break;
                default:
                    throw "Neočekivana greška.";
                    break;
            }
            break;
        default:
            throw "Neočekivana greška.";
    }
    $(".field").each(function () {
        var coords = $(this).attr('data-coordinates').split('-').map(Number);
        var displayCoordinates = internalToAxis(type, corner, axis, {
            a: dimensions[0],
            b: dimensions[1],
            c: dimensions[2]
        }, coords[0], coords[1]);
        $(this).append("<span class='display-coordinates'>" + displayCoordinates.join('-') + "</span>");
        $(this).attr("data-display-coordinates", displayCoordinates.join('-'));
    });

    axis[0] = axis[0].join(" ");
    axis[1] = axis[1].join(" ");
};

var internalToAxis = function (type, corner, axis, dims, x, y) {
    var newX = 0, newY = 0;
    switch (type) {
        case "rectangular":
            switch (corner) {
                case "top-left":
                    newX = dims.a - x - 1;
                    newY = dims.b - y - 1;
                    break;
                case "top-right":
                    newX = x;
                    newY = dims.b - y - 1;
                    break;
                case "bottom-left":
                    newX = dims.a - x - 1;
                    newY = y;
                    break;
                case "bottom-right":
                    newX = x;
                    newY = y;
                    break;
                default:
                    throw "Neočekivana greška. " + corner + " nije validna vrednost.";
                    break;
            }
            break;
        case "hexagonal-flat":
            switch (corner) {
                case "right":
                    newX = x - y + dims.b - 1;
                    newY = dims.a + dims.c - y;
                    break;
                case "bottom-right":
                    newX = -y + dims.b + dims.c - 2;
                    newY = -x + dims.a + dims.c - 2;
                    break;
                case "bottom-left":
                    newX = -x + dims.a + dims.c - 2;
                    newY = y - x + dims.a - 1;
                    break;
                case "top-left":
                    //noinspection JSSuspiciousNameCombination
                    newX = y;
                    //noinspection JSSuspiciousNameCombination
                    newY = x;
                    break;
                case "top-right":
                    newX = x;
                    newY = x - y + dims.b - 1;
                    break;
                case "left":
                    newX = y - x + dims.a - 1;
                    newY = y;
                    break;
                default:
                    throw "Neočekivana greška. " + corner + "nije validna vrednost.";
            }
            break;
        case "hexagonal-pointy":
            switch (corner) {
                case "top":
                    //noinspection JSSuspiciousNameCombination
                    newX = y;
                    //noinspection JSSuspiciousNameCombination
                    newY = x;
                    break;
                case "bottom":
                    newX = -y + dims.b + dims.c - 2;
                    newY = -x + dims.a + dims.c - 2;
                    break;
                case "top-left":
                    newX = y - x + dims.a - 1;
                    newY = y;
                    break;
                case "top-right":
                    newX = x;
                    newY = x - y + dims.b - 1;
                    break;
                case "bottom-left":
                    newX = -x + dims.c + dims.a - 2;
                    newY = y - x + dims.a - 1;
                    break;
                case "bottom-right":
                    newX = x - y + dims.b - 1;
                    newY = -y + dims.c + dims.b - 2;
                    break;
            }
            break;
        default:
            throw "Neočekviana greška.";
    }
    //return [newX, newY];
    return [axis[0][newX], axis[1][newY]];
};

var displayAxisHexagonalFlatBottomLeft = function (axis, dimensions) {
    var dims = {a: dimensions[0], b: dimensions[1], c: dimensions[2]};
    for (var i = 0; i <= dims.c; i++) {
        var pos = hexagonalFlatCoordinatesToPosition(dims.a + i - 1, i - 1, fieldSize + 2 * fieldMargin, dims);
        $("<div/>", {
            class: "axis-field",
            style: 'left: ' + pos[0] + 'px; top: ' + pos[1] + 'px; height: ' + fieldSize / Math.sqrt(3) + 'px; width: ' + fieldSize + 'px;'
        })
            .append("<span>" + axis[i] + "</span>")
            .appendTo($(".board"));
    }
};

var displayAxisHexagonalFlatTopRight = function (axis, dimensions) {
    var dims = {a: dimensions[0], b: dimensions[1], c: dimensions[2]};
    for (var i = 0; i <= dims.c; i++) {
        var pos = hexagonalFlatCoordinatesToPosition(i - 1, dims.b + i - 1, fieldSize + 2 * fieldMargin, dims);
        $("<div/>", {
            class: "axis-field",
            style: 'left: ' + pos[0] + 'px; top: ' + pos[1] + 'px; height: ' + fieldSize / Math.sqrt(3) + 'px; width: ' + fieldSize + 'px;'
        })
            .append("<span>" + axis[i] + "</span>")
            .appendTo($(".board"));
    }
};

var displayAxisHexagonalFlatTopLeft = function (axis, dimensions) {
    var dims = {a: dimensions[0], b: dimensions[1], c: dimensions[2]};
    for (var i = 0; i <= dims.a; i++) {
        var pos = hexagonalFlatCoordinatesToPosition(i - 1, -1, fieldSize + 2 * fieldMargin, dims);
        $("<div/>", {
            class: "axis-field",
            style: 'left: ' + pos[0] + 'px; top: ' + pos[1] + 'px; height: ' + fieldSize / Math.sqrt(3) + 'px; width: ' + fieldSize + 'px;'
        })
            .append("<span>" + axis[i] + "</span>")
            .appendTo($(".board"));
    }
};

var displayAxisHexagonalFlatTop = function (axis, dimensions) {
    var dims = {a: dimensions[0], b: dimensions[1], c: dimensions[2]};
    for (var i = 0; i <= dims.b; i++) {
        var pos = hexagonalFlatCoordinatesToPosition(-1, i - 1, fieldSize + 2 * fieldMargin, dims);
        $("<div/>", {
            class: "axis-field",
            style: 'left: ' + pos[0] + 'px; top: ' + pos[1] + 'px; height: ' + fieldSize / Math.sqrt(3) + 'px; width: ' + fieldSize + 'px;'
        })
            .append("<span>" + axis[i] + "</span>")
            .appendTo($(".board"));
    }
};

var displayAxisHexagonalFlatBottom = function (axis, dimensions) {
    var dims = {a: dimensions[0], b: dimensions[1], c: dimensions[2]};
    for (var i = 0; i <= dims.b; i++) {
        var pos = hexagonalFlatCoordinatesToPosition(dims.a + dims.c - 1, dims.c + i - 1, fieldSize + 2 * fieldMargin, dims);
        $("<div/>", {
            class: "axis-field",
            style: 'left: ' + pos[0] + 'px; top: ' + pos[1] + 'px; height: ' + fieldSize / Math.sqrt(3) + 'px; width: ' + fieldSize + 'px;'
        })
            .append("<span>" + axis[i] + "</span>")
            .appendTo($(".board"));
    }
};

var displayAxisHexagonalFlatBottomRight = function (axis, dimensions) {
    var dims = {a: dimensions[0], b: dimensions[1], c: dimensions[2]};
    for (var i = 0; i <= dims.a; i++) {
        var pos = hexagonalFlatCoordinatesToPosition(dims.a + dims.c - 1 - i, dims.b + dims.c - 1, fieldSize + 2 * fieldMargin, dims);
        $("<div/>", {
            class: "axis-field",
            style: 'left: ' + pos[0] + 'px; top: ' + pos[1] + 'px; height: ' + fieldSize / Math.sqrt(3) + 'px; width: ' + fieldSize + 'px;'
        })
            .append("<span>" + axis[i] + "</span>")
            .appendTo($(".board"));
    }
};

var displayAxisHexagonalPointyLeft = function (axis, dimensions) {
    var dims = {a: dimensions[0], b: dimensions[1], c: dimensions[2]};
    for (var i = 0; i <= dims.c; i++) {
        var pos = hexagonalPointyCoordinatesToPosition(dims.a + i - 1, i - 1, fieldSize + 2 * fieldMargin, dims);
        $("<div/>", {
            class: "axis-field",
            style: 'left: ' + pos[0] + 'px; top: ' + pos[1] + 'px; height: ' + fieldSize + 'px; width: ' + fieldSize / Math.sqrt(3) + 'px;'
        })
            .append("<span>" + axis[i] + "</span>")
            .appendTo($(".board"));
    }
};

var displayAxisHexagonalPointyRight = function (axis, dimensions) {
    var dims = {a: dimensions[0], b: dimensions[1], c: dimensions[2]};
    for (var i = 0; i <= dims.c; i++) {
        var pos = hexagonalPointyCoordinatesToPosition(i - 1, dims.b + i - 1, fieldSize + 2 * fieldMargin, dims);
        $("<div/>", {
            class: "axis-field",
            style: 'left: ' + pos[0] + 'px; top: ' + pos[1] + 'px; height: ' + fieldSize + 'px; width: ' + fieldSize / Math.sqrt(3) + 'px;'
        })
            .append("<span>" + axis[i] + "</span>")
            .appendTo($(".board"));
    }
};

var displayAxisHexagonalPointyTopLeft = function (axis, dimensions) {
    var dims = {a: dimensions[0], b: dimensions[1], c: dimensions[2]};
    for (var i = 0; i <= dims.a; i++) {
        var pos = hexagonalPointyCoordinatesToPosition(i - 1, -1, fieldSize + 2 * fieldMargin, dims);
        $("<div/>", {
            class: "axis-field",
            style: 'left: ' + pos[0] + 'px; top: ' + pos[1] + 'px; height: ' + fieldSize + 'px; width: ' + fieldSize / Math.sqrt(3) + 'px;'
        })
            .append("<span>" + axis[i] + "</span>")
            .appendTo($(".board"));
    }
};

var displayAxisHexagonalPointyTopRight = function (axis, dimensions) {
    var dims = {a: dimensions[0], b: dimensions[1], c: dimensions[2]};
    for (var i = 0; i <= dims.b; i++) {
        var pos = hexagonalPointyCoordinatesToPosition(-1, i - 1, fieldSize + 2 * fieldMargin, dims);
        $("<div/>", {
            class: "axis-field",
            style: 'left: ' + pos[0] + 'px; top: ' + pos[1] + 'px; height: ' + fieldSize + 'px; width: ' + fieldSize / Math.sqrt(3) + 'px;'
        })
            .append("<span>" + axis[i] + "</span>")
            .appendTo($(".board"));
    }
};

var displayAxisHexagonalPointyBottomLeft = function (axis, dimensions) {
    var dims = {a: dimensions[0], b: dimensions[1], c: dimensions[2]};
    for (var i = 0; i <= dims.b; i++) {
        var pos = hexagonalPointyCoordinatesToPosition(dims.a + dims.c - 1, dims.c + i - 1, fieldSize + 2 * fieldMargin, dims);
        $("<div/>", {
            class: "axis-field",
            style: 'left: ' + pos[0] + 'px; top: ' + pos[1] + 'px; height: ' + fieldSize + 'px; width: ' + fieldSize / Math.sqrt(3) + 'px;'
        })
            .append("<span>" + axis[i] + "</span>")
            .appendTo($(".board"));
    }
};

var displayAxisHexagonalPointyBottomRight = function (axis, dimensions) {
    var dims = {a: dimensions[0], b: dimensions[1], c: dimensions[2]};
    for (var i = 0; i <= dims.a; i++) {
        var pos = hexagonalPointyCoordinatesToPosition(dims.a + dims.c - 1 - i, dims.b + dims.c - 1, fieldSize + 2 * fieldMargin, dims);
        $("<div/>", {
            class: "axis-field",
            style: 'left: ' + pos[0] + 'px; top: ' + pos[1] + 'px; height: ' + fieldSize + 'px; width: ' + fieldSize / Math.sqrt(3) + 'px;'
        })
            .append("<span>" + axis[i] + "</span>")
            .appendTo($(".board"));
    }
};

var drawChess = function () {
    var $board = $(".board");
    var $fields = $board.children(".field");

    var dataBoardType = $board.attr('data-board-type');

    switch (dataBoardType) {
        case "hexagonal-flat":
        case "hexagonal-pointy":
            $fields.each(function () {
                var coords = $(this).attr("data-coordinates").split('-').map(Number);
                switch ((coords[0] + coords[1]) % 3) {
                    case 0:
                        $(this).addClass("chess-white");
                        break;
                    case 1:
                        $(this).addClass("chess-middle");
                        break;
                    case 2:
                        $(this).addClass("chess-black");
                        break;
                    default:
                        throw "Maths has been broken."
                }
            });
            break;

        case "rectangular":
            $fields.each(function () {
                var dataCoordinates = $(this).attr("data-coordinates").split('-');
                if ((dataCoordinates[0] % 2 + dataCoordinates[1] % 2) % 2 === 0) {
                    $(this).addClass("chess-white");
                } else {
                    $(this).addClass("chess-black");
                }
            });
            break;

        default:
            throw "Neočekivana greška. Iz data-board-type pročitano " + dataBoardType + ".";
    }
};

var drawGoStyle = function () {
    var $board = $(".board");
    var $fields = $board.children(".field");

    // Determine # of rows and # of columns
    var cols = 0;
    var rows = 0;
    $fields.each(function () {
        var dataCoordinates = $(this).attr("data-coordinates").split('-').map(Number);
        if (dataCoordinates[1] > cols) cols = dataCoordinates[1];
        if (dataCoordinates[0] > rows) rows = dataCoordinates[0];
    });

    $fields.each(function () {
        var dataCoordinates = $(this).attr("data-coordinates").split('-').map(Number);
        if (dataCoordinates[0] === 0 && dataCoordinates[1] === 0) {
            $(this).addClass("go-style--corner-t-l");
        } else if (dataCoordinates[0] === 0 && dataCoordinates[1] === cols) {
            $(this).addClass("go-style--corner-t-r");
        } else if (dataCoordinates[0] === rows && dataCoordinates[1] === 0) {
            $(this).addClass("go-style--corner-b-l");
        } else if (dataCoordinates[0] === rows && dataCoordinates[1] === cols) {
            $(this).addClass("go-style--corner-b-r");
        } else if (dataCoordinates[0] === 0) {
            $(this).addClass("go-style--edge-l");
        } else if (dataCoordinates[0] === rows) {
            $(this).addClass("go-style--edge-r");
        } else if (dataCoordinates[1] === 0) {
            $(this).addClass("go-style--edge-t");
        } else if (dataCoordinates[1] === cols) {
            $(this).addClass("go-style--edge-b");
        } else {
            $(this).addClass("go-style--middle");
        }
    });
};

var drawCircleStyle = function () {
    var $fields = $('.board').children('.field');
    $fields.each(function () {
        $(this).addClass("circle-style size-" + fieldSizeName);
    });
};


// --------------------------------------- //
// -------------  DATA STATE ------------- //
// --------------------------------------- //

var displayDataState = function (state) {
    // foreach state
    for (var i = 0; i < state.length; i++) {
        // foreach field
        for (var j = 0; j < state[i].fields.length; j++) {
            var query = state[i].fields[j][0] + '-' + state[i].fields[j][1];
            var $field = $("[data-display-coordinates=" + query + "]");
            if (!$field.length) {
                displayWarning("Dobijen zahtev da se na polje *" + query +
                    "* smesti oblik `" + state[i].style.shape +
                    "` boje `" + state[i].style.color + "`. " +
                    "Ovo polje nije deo definisane table. Ignoriše se.");
            }
            if ($field.hasClass("occupied")) {
                // get the shape it already has
                var theColor = "";
                var theShape = "";
                var classes = $field.children(".piece").attr("class").split(' ');
                $.each(classes, function (i, e) {
                    if (e.indexOf("shape--") === 0) {
                        theShape = e.substring(7);
                    }
                    if (e.indexOf("color--") === 0) {
                        theColor = e.substring(7);
                    }
                });
                displayWarning("Dobijen zahtev da se na polje *" + query +
                    "* smesti oblik `" + state[i].style.shape +
                    "` boje `" + state[i].style.color + "`. " +
                    "Ovo polje već na sebi ima figuru oblika `" +
                    theShape + "` boje `" + theColor + "`."
                );
            }
            $field.addClass("occupied");
            $('<div/>', {
                class: 'piece color--' + state[i].style.color + ' ' + 'shape--' + state[i].style.shape
            }).appendTo($field);
        }
    }
};

// --------------------------------------- //
// ------------  DATA MARKINGS ----------- //
// --------------------------------------- //

var displayDataMarkings = function (markings) {
    //foreach marking
    for (var i = 0; i < markings.length; i++) {
        //foreach field
        for (var j = 0; j < markings[i].fields.length; j++) {
            var query = markings[i].fields[j][0] + '-' + markings[i].fields[j][1];
            var $field = $("[data-display-coordinates=" + query + "]");
            if (!$field.length) {
                displayWarning("Dobijen zahtev da se na polje " + query +
                    " smesti oznaka \"" + markings[i].style.shape +
                    "\". Ovo polje nije deo definisane table. Ignoriše se.");
            }
            $field.addClass("marked");
            $("<div/>", {
                class: 'marking color--' + markings[i].style.color + ' ' + 'shape--' + markings[i].style.shape
            }).appendTo($field);
        }
    }
};

// --------------------------------------- //
// -------------  DATA PLAYER ------------ //
// --------------------------------------- //

var displayDataPlayer = function (player) {
    var order = player.order + "";
    var query = "player" + order;
    var $player = $("." + query);

    var $name = $player.children(".name");
    $name.text(player.name);

    var $message = $player.children(".message");
    $message.removeClass("show");
    if (!player.message) {
        $message.addClass("show");
    }
    $message.text(player.message);
};

// --------------------------------------- //
// -------------  DATA REMOVED ----------- //
// --------------------------------------- //

var displayDataRemoved = function (removed) {
    // validate
    if (removed.length != 2) {
        throw "Neočekivana greška. Niz \"removed\" mora imati tačno dva podniza. " +
        "Dobijena duzina je " + removed.length + ".";
    }
    // foreach player
    for (var i = 0; i < removed.length; i++) {
        var $player = $(".player" + (i + 1));
        var $removed = $player.children(".removed");
        // clean first
        $removed.html('');
        // for each removed figure
        for (var j = 0; j < removed[i].length; j++) {
            // draw a figure NUMBER times
            for (var k = 0; k < removed[i][j].number; k++) {
                $("<div/>", {
                    class: 'piece piece-removed color--' + removed[i][j].style.color + ' ' + 'shape--' + removed[i][j].style.shape
                }).appendTo($removed);
            }
        }
    }
};

// Very simple markdown parser.
// Doesn't allow nested formatting.
var parseMarkdown = function (message) {
    var currentMode = ""; // Bold, Italic, Code (in order: *, _, `)
    var output = "";
    for (var i = 0; i < message.length; i++) {
        switch (message[i]) {
            case "*":
                if (currentMode === "b") {
                    output += "</strong>";
                    currentMode = "";
                } else {
                    output += "<strong>";
                    currentMode = "b";
                }
                break;
            case "_":
                if (currentMode === "i") {
                    output += "</em>";
                    currentMode = "";
                } else {
                    output += "<em>";
                    currentMode = "i";
                }
                break;
            case "`":
                if (currentMode === "c") {
                    output += "</code>";
                    currentMode = "";
                } else {
                    output += "<code>";
                    currentMode = "c";
                }
                break;
            default:
                output += message[i];
        }
    }
    return output;
};

var displayError = function (message) {
    displayMessage(message, "ERROR");
};

var displayWarning = function (message) {
    console.warn(message);
    displayMessage(message, "WARNING");
};

var displayMessage = function (message, type) {
    type = type || "INFO";
    messages.push({
        type: type,
        message: message
    });
    displayMessages();
};

var displayMessages = function () {
    var $messages = $("#messages");

    // remove all messages
    $messages.html("");

    // display all messages
    for (let i = 0; i < messages.length; i++) {
        $("<div/>", {
            class: "message message-" + messages[i].type.toLowerCase(),
            "data-index": i
        })
            .append("<span>" + parseMarkdown(messages[i].message) + "</span>")
            .append("<div class='message-close'></div>")
            .appendTo($messages);
    }

    // if more than one message, create a "dissmiss all" button
    if (messages.length >= 2) {
        $("<div/>", {
            id: "dismiss-all-messages"
        })
            .append("<span>Ukloni sve (<code>q</code>)</span>")
            .prependTo($messages);
    }
};

var dismissMessage = function (index) {
    messages.splice(index, 1);
    displayMessages();
};

var dismissAllMessages = function () {
    messages = [];
    displayMessages();
};

var $messages = $("#messages");

$messages.on("click", ".message-close", function () {
    dismissMessage($(this).parent().attr("data-index"));
});

$messages.on("click", "#dismiss-all-messages", dismissAllMessages);
$(document).bind("keyup", "q", dismissAllMessages);

$(document).ready(function () {
    //displayMessage("TutiFruti :: version 0.0");
    displayData(data1);
});

var example = {};

example.chess = {
    board: {
        type: "rectangular",
        dimensions: [8, 8],
        corner: "bottom-left",
        axis: [
            "1 2 3 4 5 6 7 8",
            "A B C D E F G H"
        ],
        //enumerationStyle: "out",
        coloring: "chess",
        mode: "classic",
        size: "xxl"
    },
    player: {
        name: "La Plávusha",
        order: 1,
        message: "TutiFruti je awesome ♥"
    },
    state: [
        {
            // Crni pešaci
            fields: [
                ["7", "A"], ["7", "B"], ["7", "C"], ["7", "D"], ["7", "E"], ["7", "F"], ["7", "G"], ["7", "H"]
            ],
            style: {
                color: "black",
                shape: "chess-pawn-fill"
            }
        },
        {
            // Beli pešaci
            fields: [
                ["2", "A"], ["2", "B"], ["2", "C"], ["2", "D"], ["2", "E"], ["2", "F"], ["2", "G"], ["2", "H"]
            ],
            style: {
                color: "white",
                shape: "chess-pawn-fill"
            }
        },
        {
            // Crni topovi
            fields: [
                ["8", "A"], ["8", "H"]
            ],
            style: {
                color: "black",
                shape: "chess-rook-fill"
            }
        },
        {
            fields: [
                ["8", "B"], ["8", "G"]
            ],
            style: {
                color: "black",
                shape: "chess-knight-fill"
            }
        },
        {
            fields: [
                ["8", "C"], ["8", "F"]
            ],
            style: {
                color: "black",
                shape: "chess-bishop-fill"
            }
        },
        {
            fields: [
                ["8", "D"]
            ],
            style: {
                color: "black",
                shape: "chess-queen-fill"
            }
        },
        {
            fields: [
                ["8", "E"]
            ],
            style: {
                color: "black",
                shape: "chess-king-fill"
            }
        },
        {
            // Beli topovi
            fields: [
                ["1", "A"], ["1", "H"]
            ],
            style: {
                color: "white",
                shape: "chess-rook-fill"
            }
        },
        {
            fields: [
                ["1", "B"], ["1", "G"]
            ],
            style: {
                color: "white",
                shape: "chess-knight-fill"
            }
        },
        {
            fields: [
                ["1", "C"], ["1", "F"]
            ],
            style: {
                color: "white",
                shape: "chess-bishop-fill"
            }
        },
        {
            fields: [
                ["1", "D"]
            ],
            style: {
                color: "white",
                shape: "chess-queen-fill"
            }
        },
        {
            fields: [
                ["1", "E"]
            ],
            style: {
                color: "white",
                shape: "chess-king-fill"
            }
        }
    ],
    markings: [
        {
            fields: [
                ["4", "D"],
                ["3", "D"]
            ],
            style: {
                color: "blue",
                shape: "circle"
            }
        },
        {
            fields: [
                ["2", "D"]
            ],
            style: {
                color: "yellow",
                shape: "O"
            }
        }
    ]
};
example.hexaChess = {
    board: {
        type: "hexagonal-pointy",
        dimensions: [5, 5, 6],
        corner: "bottom-left",
        axis: [
            "A B C D E F G H I J K L M N O P Q",
            "1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20"
        ],
        coloring: "chess",
        mode: "classic",
        size: "xxl"
    },
    player: {
        name: "La Plávusha",
        order: 1,
        message: "TutiFruti je awesome ♥"
    },
    state: [
        {
            // Crni pešaci
            fields: [
                ["E", "1"], ["F", "2"], ["G", "3"], ["H", "4"], ["I", "5"],
                ["I", "6"], ["I", "7"], ["I", "8"], ["I", "9"]
            ],
            style: {
                color: "black",
                shape: "chess-pawn-fill"
            }
        },
        {
            // Beli pešaci
            fields: [
                ["B", "1"], ["B", "2"], ["B", "3"], ["B", "4"], ["B", "5"],
                ["C", "6"], ["D", "7"], ["E", "8"], ["F", "9"]
            ],
            style: {
                color: "white",
                shape: "chess-pawn-fill"
            }
        },
        {
            // Crni topovi
            fields: [
                ["F", "1"], ["J", "9"]
            ],
            style: {
                color: "black",
                shape: "chess-rook-fill"
            }
        },
        {
            fields: [
                ["H", "3"], ["J", "8"]
            ],
            style: {
                color: "black",
                shape: "chess-knight-fill"
            }
        },
        {
            fields: [
                ["G", "2"], ["J", "7"], ["I", "4"]
            ],
            style: {
                color: "black",
                shape: "chess-bishop-fill"
            }
        },
        {
            fields: [
                ["J", "6"]
            ],
            style: {
                color: "black",
                shape: "chess-queen-fill"
            }
        },
        {
            fields: [
                ["J", "5"]
            ],
            style: {
                color: "black",
                shape: "chess-king-fill"
            }
        },
        {
            // Beli topovi
            fields: [
                ["A", "1"], ["E", "9"]
            ],
            style: {
                color: "white",
                shape: "chess-rook-fill"
            }
        },
        {
            fields: [
                ["A", "2"], ["C", "7"]
            ],
            style: {
                color: "white",
                shape: "chess-knight-fill"
            }
        },
        {
            fields: [
                ["A", "3"], ["B", "6"], ["D", "8"]
            ],
            style: {
                color: "white",
                shape: "chess-bishop-fill"
            }
        },
        {
            fields: [
                ["A", "4"]
            ],
            style: {
                color: "white",
                shape: "chess-queen-fill"
            }
        },
        {
            fields: [
                ["A", "5"]
            ],
            style: {
                color: "white",
                shape: "chess-king-fill"
            }
        }
    ],
    markings: [
        {
            fields: [
                ["F", "5"]
            ],
            style: {
                color: "pink",
                shape: "star"
            }
        }
    ]
};
example.gomokuWestern = {
    board: {
        type: "rectangular",
        dimensions: [15, 15],
        corner: "bottom-left",
        axis: [
            "1 2 3 4 5 6 7 8 9 10 11 12 13 14 15",
            "A B C D E F G H I J K L M N O"
        ],
        coloring: "classic",
        mode: "classic"
    },
    player: {
        name: "La Plávusha",
        order: 1,
        message: "TutiFruti je awesome ♥"
    },
    state: [
        {
            fields: [
                ["13", "C"], ["12", "D"], ["11", "E"], ["11", "F"], ["11", "G"], ["11", "H"], ["11", "J"], ["11", "L"],
                ["12", "M"], ["10", "F"], ["10", "G"], ["10", "I"], ["10", "K"], ["9", "F"], ["9", "H"], ["9", "J"],
                ["9", "L"], ["8", "E"], ["8", "F"], ["8", "G"], ["8", "H"], ["8", "M"], ["7", "E"], ["7", "H"],
                ["6", "D"], ["6", "E"], ["6", "I"], ["6", "L"], ["5", "E"], ["5", "H"], ["5", "K"], ["4", "F"],
                ["4", "J"], ["3", "D"]
            ],
            style: {
                color: "red",
                shape: "O"
            }
        },
        {
            fields: [
                ["14", "B"], ["13", "F"], ["12", "E"], ["12", "F"], ["12", "I"], ["12", "K"], ["12", "N"], ["11", "C"],
                ["11", "I"], ["10", "H"], ["9", "E"], ["9", "G"], ["9", "I"], ["8", "D"], ["8", "I"], ["8", "J"],
                ["8", "K"], ["7", "C"], ["7", "D"], ["7", "F"], ["7", "G"], ["7", "I"], ["7", "L"], ["7", "M"],
                ["7", "N"], ["6", "H"], ["5", "F"], ["5", "G"], ["5", "I"], ["5", "J"], ["3", "C"], ["3", "I"],
                ["2", "H"]
            ],
            style: {
                color: "blue",
                shape: "X"
            }
        }
    ],
    markings: [
        {
            fields: [
                [1, 1],
                [1, 2]
            ],
            style: "move"
        }
    ]
};
example.gomokuEastern = {
    board: {
        type: "rectangular",
        dimensions: [15, 15],
        corner: "bottom-left",
        axis: [
            "1 2 3 4 5 6 7 8 9 10 11 12 13 14 15",
            "A B C D E F G H I J K L M N O"
        ],
        coloring: "classic",
        mode: "go"
    },
    player: {
        name: "La Plávusha",
        order: 1,
        message: "TutiFruti je awesome ♥"
    },
    state: [
        {
            fields: [
                ["13", "C"], ["12", "D"], ["11", "E"], ["11", "F"], ["11", "G"], ["11", "H"], ["11", "J"], ["11", "L"],
                ["12", "M"], ["10", "F"], ["10", "G"], ["10", "I"], ["10", "K"], ["9", "F"], ["9", "H"], ["9", "J"],
                ["9", "L"], ["8", "E"], ["8", "F"], ["8", "G"], ["8", "H"], ["8", "M"], ["7", "E"], ["7", "H"],
                ["6", "D"], ["6", "E"], ["6", "I"], ["6", "L"], ["5", "E"], ["5", "H"], ["5", "K"], ["4", "F"],
                ["4", "J"], ["3", "D"]
            ],
            style: {
                color: "black",
                shape: "circle"
            }
        },
        {
            fields: [
                ["14", "B"], ["13", "F"], ["12", "E"], ["12", "F"], ["12", "I"], ["12", "K"], ["12", "N"], ["11", "C"],
                ["11", "I"], ["10", "H"], ["9", "E"], ["9", "G"], ["9", "I"], ["8", "D"], ["8", "I"], ["8", "J"],
                ["8", "K"], ["7", "C"], ["7", "D"], ["7", "F"], ["7", "G"], ["7", "I"], ["7", "L"], ["7", "M"],
                ["7", "N"], ["6", "H"], ["5", "F"], ["5", "G"], ["5", "I"], ["5", "J"], ["3", "C"], ["3", "I"],
                ["2", "H"]
            ],
            style: {
                color: "white",
                shape: "circle"
            }
        }
    ],
    markings: [
        {
            fields: [
                [1, 1],
                [1, 2]
            ],
            style: "move"
        }
    ]
};


//TODO pomeri odavde
$('#button').click(function () {
    $('.info').toggleClass('hidden');
    $('.generator').toggleClass('hidden');
});
