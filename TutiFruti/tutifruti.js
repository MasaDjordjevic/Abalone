const fieldSize = 42; // in pixels
const fieldMargin = 0; // in pixels

var data1 = {
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
                shape: "X"
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
                shape: "O"
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

var displayData = function(data) {
    try {
        displayDataBoard(data.board);
        displayDataState(data.state);
        displayDataPlayer(data.player);
    } catch (ex) {
        console.error(ex);
        displayError(ex);
    }
};


// --------------------------------------- //
// -------------  DATA BOARD ------------- //
// --------------------------------------- //


var displayDataBoard = function(dataBoard) {
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
        case "triangular-flat":
            if (dataBoard.dimensions.length < 1)
                throw ("Tabla je trougaona, a broj dobijenih dimenzija je " +
                dataBoard.dimensions.length + ". Treba da bude 1.");
            if (dataBoard.dimensions.length > 1)
                displayWarning("Tabla je trougaona, a broj dobijenih dimenzija je " +
                    dataBoard.dimensions.length + ". Koristim samo prve 1 dimenzije, ostale ignorišem.");
            drawTriangularFlatBoard(dataBoard.dimensions[0]);
            break;
        case "triangular-pointy":
            if (dataBoard.dimensions.length < 1)
                displayWarning("Tabla je trougaona, a broj dobijenih dimenzija je " +
                    dataBoard.dimensions.length + ". Treba da bude 1.");
            if (dataBoard.dimensions.length > 1)
                displayWarning("Tabla je trougaona, a broj dobijenih dimenzija je " +
                    dataBoard.dimensions.length + ". Koristim samo prve 1 dimenzije, ostale ignorišem.");
            drawTriangularPointyBoard(dataBoard.dimensions[0]);
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

var drawRectangularBoard = function(rows, cols) {
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
            class: 'field rectangle',
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

var hexagonalFlatCoordinatesToPosition = function(x, y, size, dimension) {
    size = size / Math.sqrt(3) / 2;
    var cubeZ = x;
    var cubeY = -y;
    var cubeX = -(cubeY + cubeZ);
    cubeX -= (dimension.a + dimension.b - 1) / 2 - dimension.a + 1;
    cubeZ -= (dimension.c + dimension.a - 1) / 2;
    var posX = 2 * size * Math.sqrt(3) * (cubeX + cubeZ / 2);
    var posY = 2 * size * (3/2) * cubeZ;
    posY += size / 2;
    posX += size * Math.sqrt(3) / 2;
    return [posX, posY];
};

var drawHexagonalFlatBoard = function(a, b, c) {
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
                class: 'field hexagon-pointy',
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
                class: 'field hexagon-pointy',
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
                class: 'field hexagon-pointy',
                "data-coordinates": '' + i + '-' + j,
                style: 'left: ' + pos[0] + 'px; top: ' + pos[1] + 'px;'
            })
                .append('<span>' + i + '-' + j + '</span>')
                .appendTo($board);
        }
        o++; // increase offset
    }
};

var hexagonalPointyCoordinatesToPosition = function(x, y, size, dimension) {
    size = size / Math.sqrt(3) / 2;
    var cubeZ = x;
    var cubeY = -y;
    var cubeX = -(cubeY + cubeZ);
    cubeX -= (dimension.a + dimension.b - 1) / 2 - dimension.a + 1;
    cubeZ -= (dimension.c + dimension.a - 1) / 2;
    var posY = 2 * size * Math.sqrt(3) * (cubeZ + cubeX / 2);
    var posX = 2 * size * (3/2) * cubeX;
    posY += size / 2;
    posX += size * Math.sqrt(3) / 2;
    return [posX, posY];
};

var drawHexagonalPointyBoard = function(a, b, c) {
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
                class: 'field hexagon-flat',
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
                class: 'field hexagon-flat',
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
                class: 'field hexagon-flat',
                "data-coordinates": '' + i + '-' + j,
                style: 'left: ' + pos[0] + 'px; top: ' + pos[1] + 'px;'
            })
                .append('<span>' + i + '-' + j + '</span>')
                .appendTo($board);
        }
        o++; // increase offset
    }
};

var drawTriangularFlatBoard = function(size) {
    throw "TODO drawTriangularFlatBoard";
};

var drawTriangularPointyBoard = function(size) {
    throw "TODO drawTriangularPointyBoard";
};

var drawAxis = function(type, dimensions, axis, corner) {
    //displayWarning("TODO: corner za hexa/triangle");
    var i = 0;
    if (axis.length !== 2) {
        throw "Očekuju se tačno dve liste simbola za označavanje."
    }
    var $board = $('.board');
    axis[0] = axis[0].split(' ');
    axis[1] = axis[1].split(' ');
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
                    cornerOffsetXRows = 1; cornerOffsetYRows = 1;
                    cornerMirrorXRows = -1; cornerMirrorYRows = -1;
                    cornerOffsetXCols = 0; cornerOffsetYCols = 0;
                    cornerMirrorXCols = 1; cornerMirrorYCols = 1;
                    break;
                case "bottom-right":
                    cornerOffsetXRows = 0; cornerOffsetYRows = 0;
                    cornerMirrorXRows = 1; cornerMirrorYRows = 1;
                    cornerOffsetXCols = 0; cornerOffsetYCols = 0;
                    cornerMirrorXCols = 1; cornerMirrorYCols = 1;
                    axis[0] = axis[0].reverse();
                    axis[1] = axis[1].reverse();
                    break;
                case "top-left":
                    cornerOffsetXRows = 1; cornerOffsetYRows = 1;
                    cornerMirrorXRows = -1; cornerMirrorYRows = -1;
                    cornerOffsetXCols = 1; cornerOffsetYCols = 1;
                    cornerMirrorXCols = -1; cornerMirrorYCols = -1;
                    axis[0] = axis[0].reverse();
                    axis[1] = axis[1].reverse();
                    break;
                case "top-right":
                    cornerOffsetXRows = 0; cornerOffsetYRows = 0;
                    cornerMirrorXRows = 1; cornerMirrorYRows = 1;
                    cornerOffsetXCols = 1; cornerOffsetYCols = 1;
                    cornerMirrorXCols = -1; cornerMirrorYCols = -1;
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
            var firstHalf, secondHalf;
            switch (corner) {
                case "top-left":
                    // clockwise
                    firstHalf = axis[0].slice(0, dimensions[1]);
                    secondHalf = axis[0].slice(dimensions[1], dimensions[1] + dimensions[2] - 1);
                    firstHalf.unshift(" ");
                    secondHalf.push(" "); secondHalf.unshift(" ");
                    displayAxisHexagonalFlatTop(firstHalf, dimensions);
                    displayAxisHexagonalFlatTopRight(secondHalf, dimensions);
                    // counter-clockwise
                    firstHalf =axis[1].slice(0, dimensions[0]);
                    secondHalf = axis[1].slice(dimensions[0], dimensions[0] + dimensions[2] - 1);
                    firstHalf.unshift(" ");
                    secondHalf.push(" "); secondHalf.unshift(" ");
                    displayAxisHexagonalFlatTopLeft(firstHalf, dimensions);
                    displayAxisHexagonalFlatBottomLeft(secondHalf, dimensions);
                    break;
                case "bottom-left":
                    // clockwise
                    firstHalf = axis[0].slice(0, dimensions[2]);
                    secondHalf = axis[0].slice(dimensions[2], dimensions[2] + dimensions[0] - 1);
                    firstHalf.unshift(" ");
                    secondHalf.push(" "); secondHalf.unshift(" ");
                    displayAxisHexagonalFlatBottomLeft(firstHalf.reverse(), dimensions);
                    displayAxisHexagonalFlatTopLeft(secondHalf.reverse(), dimensions);
                    // counter-clockwise
                    firstHalf =axis[1].slice(0, dimensions[1]);
                    secondHalf = axis[1].slice(dimensions[1], dimensions[1] + dimensions[0] - 1);
                    firstHalf.unshift(" ");
                    secondHalf.push(" "); secondHalf.unshift(" ");
                    displayAxisHexagonalFlatBottom(firstHalf, dimensions);
                    displayAxisHexagonalFlatBottomRight(secondHalf, dimensions);
                    break;
                case "top-right":
                    // clockwise
                    firstHalf = axis[0].slice(0, dimensions[2]);
                    secondHalf = axis[0].slice(dimensions[2], dimensions[2] + dimensions[0] - 1);
                    firstHalf.unshift(" ");
                    secondHalf.push(" "); secondHalf.unshift(" ");
                    displayAxisHexagonalFlatTopRight(firstHalf, dimensions);
                    displayAxisHexagonalFlatBottomRight(secondHalf, dimensions);
                    // counter-clockwise
                    firstHalf =axis[1].slice(0, dimensions[1]);
                    secondHalf = axis[1].slice(dimensions[1], dimensions[1] + dimensions[0] - 1);
                    firstHalf.unshift(" ");
                    secondHalf.push(" "); secondHalf.unshift(" ");
                    displayAxisHexagonalFlatTop(firstHalf.reverse(), dimensions);
                    displayAxisHexagonalFlatTopLeft(secondHalf, dimensions);
                    break;
                case "bottom-right":
                    // clockwise
                    firstHalf = axis[0].slice(0, dimensions[1]);
                    secondHalf = axis[0].slice(dimensions[1], dimensions[1] + dimensions[2] - 1);
                    firstHalf.unshift(" ");
                    secondHalf.push(" "); secondHalf.unshift(" ");
                    displayAxisHexagonalFlatBottom(firstHalf.reverse(), dimensions);
                    displayAxisHexagonalFlatBottomLeft(secondHalf.reverse(), dimensions);
                    // counter-clockwise
                    firstHalf =axis[1].slice(0, dimensions[0]);
                    secondHalf = axis[1].slice(dimensions[0], dimensions[0] + dimensions[2] - 1);
                    firstHalf.unshift(" ");
                    secondHalf.push(" "); secondHalf.unshift(" ");
                    displayAxisHexagonalFlatBottomRight(firstHalf, dimensions);
                    displayAxisHexagonalFlatTopRight(secondHalf.reverse(), dimensions);
                    break;
                case "right":
                    // clockwise
                    firstHalf = axis[0].slice(0, dimensions[0]);
                    secondHalf = axis[0].slice(dimensions[0], dimensions[0] + dimensions[1] - 1);
                    firstHalf.unshift(" ");
                    secondHalf.push(" "); secondHalf.unshift(" ");
                    displayAxisHexagonalFlatBottomRight(firstHalf.reverse(), dimensions);
                    displayAxisHexagonalFlatBottom(secondHalf.reverse(), dimensions);
                    // counter-clockwise
                    firstHalf =axis[1].slice(0, dimensions[2]);
                    secondHalf = axis[1].slice(dimensions[2], dimensions[2] + dimensions[1] - 1);
                    firstHalf.unshift(" ");
                    secondHalf.push(" "); secondHalf.unshift(" ");
                    displayAxisHexagonalFlatTopRight(firstHalf.reverse(), dimensions);
                    displayAxisHexagonalFlatTop(secondHalf.reverse(), dimensions);
                    break;
                case "left":
                    // clockwise
                    firstHalf = axis[0].slice(0, dimensions[0]);
                    secondHalf = axis[0].slice(dimensions[0], dimensions[0] + dimensions[1] - 1);
                    firstHalf.unshift(" ");
                    secondHalf.push(" "); secondHalf.unshift(" ");
                    displayAxisHexagonalFlatTopLeft(firstHalf.reverse(), dimensions);
                    displayAxisHexagonalFlatTop(secondHalf, dimensions);
                    // counter-clockwise
                    firstHalf =axis[1].slice(0, dimensions[2]);
                    secondHalf = axis[1].slice(dimensions[2], dimensions[2] + dimensions[1] - 1);
                    firstHalf.unshift(" ");
                    secondHalf.push(" "); secondHalf.unshift(" ");
                    displayAxisHexagonalFlatBottomLeft(firstHalf, dimensions);
                    displayAxisHexagonalFlatBottom(secondHalf, dimensions);
                    break;
                default:
                    throw "Neočekivana greška.";
            }
            break;
        case "hexagonal-pointy":
            var firstHalf, secondHalf;
            switch (corner) {
                case "bottom":
                    // clockwise
                    firstHalf = axis[0].slice(0, dimensions[1]);
                    secondHalf = axis[0].slice(dimensions[1], dimensions[1] + dimensions[2] - 1);
                    firstHalf.unshift(" ");
                    secondHalf.push(" "); secondHalf.unshift(" ");
                    displayAxisHexagonalPointyBottomLeft(firstHalf.reverse(), dimensions);
                    displayAxisHexagonalPointyLeft(secondHalf.reverse(), dimensions);
                    // counter-clockwise
                    firstHalf =axis[1].slice(0, dimensions[0]);
                    secondHalf = axis[1].slice(dimensions[0], dimensions[0] + dimensions[2] - 1);
                    firstHalf.unshift(" ");
                    secondHalf.push(" "); secondHalf.unshift(" ");
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
    $(".field").each(function() {
        var coords = $(this).attr('data-coordinates').split('-').map(Number);
        var displayCoordinates = internalToAxis(type, corner, axis, {a: dimensions[0], b: dimensions[1], c: dimensions[2]}, coords[0], coords[1]);
        $(this).append("<span class='display-coordinates'>" + displayCoordinates.join('-') + "</span>");
        $(this).attr("data-display-coordinates", displayCoordinates.join('-'));
    });
};

var internalToAxis = function(type, corner, axis, dims, x, y) {
    var newX = 0, newY = 0;
    switch (type) {
        case "rectangular":
            switch(corner) {
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
                    newX = - y + dims.b + dims.c - 2;
                    newY = - x + dims.a + dims.c - 2;
                    break;
                case "bottom-left":
                    newX = - x + dims.a + dims.c - 2;
                    newY = y - x + dims.a - 1;
                    break;
                case "top-left":
                    newX = y;
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
                    newX = y;
                    newY = x;
                    break;
                case "bottom":
                    newX = - y + dims.b + dims.c - 2;
                    newY = - x + dims.a + dims.c - 2;
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
                    newX = - x + dims.c + dims.a - 2;
                    newY = y - x + dims.a - 1;
                    break;
                case "bottom-right":
                    newX = x - y + dims.b - 1;
                    newY = - y + dims.c + dims.b - 2;
                    break;
            }
            break;
        default:
            throw "Neočekviana greška.";
    }
    //return [newX, newY];
    return [axis[0][newX], axis[1][newY]];
};

var displayAxisHexagonalFlatBottomLeft = function(axis, dimensions) {
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

var displayAxisHexagonalFlatTopRight = function(axis, dimensions) {
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

var displayAxisHexagonalFlatTopLeft = function(axis, dimensions) {
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

var displayAxisHexagonalFlatTop = function(axis, dimensions) {
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

var displayAxisHexagonalFlatBottom = function(axis, dimensions) {
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

var displayAxisHexagonalFlatBottomRight = function(axis, dimensions) {
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

var displayAxisHexagonalPointyLeft = function(axis, dimensions) {
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

var displayAxisHexagonalPointyRight = function(axis, dimensions) {
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

var displayAxisHexagonalPointyTopLeft = function(axis, dimensions) {
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

var displayAxisHexagonalPointyTopRight = function(axis, dimensions) {
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

var displayAxisHexagonalPointyBottomLeft = function(axis, dimensions) {
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

var displayAxisHexagonalPointyBottomRight = function(axis, dimensions) {
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

var drawChess = function() {
    var $board = $(".board");
    var $fields = $board.children(".field");
    
    var dataBoardType = $board.attr('data-board-type');
    
    switch (dataBoardType) {
        case "hexagonal-flat":
        case "hexagonal-pointy":
            $fields.each(function() {
                var coords = $(this).attr("data-coordinates").split('-').map(Number);
                switch ((coords[0] + coords[1]) % 3) {
                    case 0: $(this).addClass("chess-white"); break;
                    case 1: $(this).addClass("chess-middle"); break;
                    case 2: $(this).addClass("chess-black"); break;
                    default: throw "Maths has been broken."
                }
            });
            break;
            
        case "rectangular":
            $fields.each(function() {
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

var drawGoStyle = function() {
    var $board = $(".board");
    var $fields = $board.children(".field");

    // Determine # of rows and # of columns
    var cols = 0; var rows = 0;
    $fields.each(function() {
        var dataCoordinates = $(this).attr("data-coordinates").split('-').map(Number);
        if (dataCoordinates[1] > cols) cols = dataCoordinates[1];
        if (dataCoordinates[0] > rows) rows = dataCoordinates[0];
    });

    $fields.each(function() {
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

var drawCircleStyle = function() {
    var $fields = $('.board').children('.field');
    $fields.each(function() {
        $(this).addClass("circle-style");
    });
};


// --------------------------------------- //
// -------------  DATA STATE ------------- //
// --------------------------------------- //

var displayDataState = function(state) {
    // foreach state
    for (var i = 0; i < state.length; i++) {
        // foreach field
        for (var j = 0; j < state[i].fields.length; j++) {
            var query = state[i].fields[j][0]+ '-' + state[i].fields[j][1];
            var $field = $("[data-display-coordinates=" + query + "]");
            if (!$field.length) {
                displayWarning("Dobijen zahtev da se na polje " + query +
                    " smesti oblik \"" + state[i].style.shape +
                    "\". Ovo polje nije deo definisane table. Ignoriše se.");
            }
            if ($field.hasClass("occupied")) {
                // get the shape it already has
                var theClass;
                var classes = $field.children(".piece").attr("class").split(' ');
                $.each(classes, function(i, e) {
                    if (e.indexOf("shape--") === 0) {
                        theClass = e;
                    }
                });
                displayWarning("Dobijen zahtev da se na polje " + query +
                    " smesti oblik \"" + state[i].style.shape +
                    "\". Ovo polje već na sebi ima figuru \"" +
                    theClass.substring(7) + "\"."
                );
            }
            $field.addClass("occupied");
            $('<div/>', {
                class: 'piece color--' + state[i].style.color + ' ' + 'shape--' + state[i].style.shape,
            }).appendTo($field);//.text('O');
            //$field.addClass("color--" + state[i].style.color);
        }
    }
};

// --------------------------------------- //
// ------------  DATA MARKINGS ----------- //
// --------------------------------------- //

var displayDataMarkings = function(markings) {

};

// --------------------------------------- //
// -------------  DATA PLAYER ------------ //
// --------------------------------------- //

var displayDataPlayer = function(player) {
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



var displayError = function(message) {
    displayMessage(message, "ERROR");
};

var displayWarning = function(message) {
    console.warn(message);
    displayMessage(message, "WARNING");
};

var displayMessage = function(message, type) {
    type = type || "INFO";
    $message = $("#message");
    $message.children("span").text(type + " :: " +message);
    $message.animate({bottom: '24px'}, 200, 'swing', function() {
        setTimeout(function() {
            $message.animate({bottom: '-100px'}, 500, 'swing');
        }, 10000);
    });
};

$(document).ready(function() {
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
        mode: "classic"
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
                shape: "chess-pawn-black"
            }
        },
        {
            // Beli pešaci
            fields: [
                ["2", "A"], ["2", "B"], ["2", "C"], ["2", "D"], ["2", "E"], ["2", "F"], ["2", "G"], ["2", "H"]
            ],
            style: {
                color: "white",
                shape: "chess-pawn-black"
            }
        },
        {
            // Crni topovi
            fields: [
                ["8", "A"], ["8", "H"]
            ],
            style: {
                color: "black",
                shape: "chess-rook-black"
            }
        },
        {
            fields: [
                ["8", "B"], ["8", "G"]
            ],
            style: {
                color: "black",
                shape: "chess-knight-black"
            }
        },
        {
            fields: [
                ["8", "C"], ["8", "F"]
            ],
            style: {
                color: "black",
                shape: "chess-bishop-black"
            }
        },
        {
            fields: [
                ["8", "D"]
            ],
            style: {
                color: "black",
                shape: "chess-queen-black"
            }
        },
        {
            fields: [
                ["8", "E"]
            ],
            style: {
                color: "black",
                shape: "chess-king-black"
            }
        },
        {
            // Beli topovi
            fields: [
                ["1", "A"], ["1", "H"]
            ],
            style: {
                color: "white",
                shape: "chess-rook-black"
            }
        },
        {
            fields: [
                ["1", "B"], ["1", "G"]
            ],
            style: {
                color: "white",
                shape: "chess-knight-black"
            }
        },
        {
            fields: [
                ["1", "C"], ["1", "F"]
            ],
            style: {
                color: "white",
                shape: "chess-bishop-black"
            }
        },
        {
            fields: [
                ["1", "D"]
            ],
            style: {
                color: "white",
                shape: "chess-queen-black"
            }
        },
        {
            fields: [
                ["1", "E"]
            ],
            style: {
                color: "white",
                shape: "chess-king-black"
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

example.hexaChess = {
    board: {
        type: "hexagonal-pointy",
        dimensions: [5, 5, 6],
        corner: "bottom-left",
        axis: [
            "A B C D E F G H I J K L M N O P Q",
            "1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20",
        ],
        coloring: "chess",
        mode: "classic"
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
                shape: "chess-pawn-black"
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
                shape: "chess-pawn-black"
            }
        },
        {
            // Crni topovi
            fields: [
                ["F", "1"], ["J", "9"]
            ],
            style: {
                color: "black",
                shape: "chess-rook-black"
            }
        },
        {
            fields: [
                ["H", "3"], ["J", "8"]
            ],
            style: {
                color: "black",
                shape: "chess-knight-black"
            }
        },
        {
            fields: [
                ["G", "2"], ["J", "7"], ["I", "4"]
            ],
            style: {
                color: "black",
                shape: "chess-bishop-black"
            }
        },
        {
            fields: [
                ["J", "6"]
            ],
            style: {
                color: "black",
                shape: "chess-queen-black"
            }
        },
        {
            fields: [
                ["J", "5"]
            ],
            style: {
                color: "black",
                shape: "chess-king-black"
            }
        },
        {
            // Beli topovi
            fields: [
                ["A", "1"], ["E", "9"]
            ],
            style: {
                color: "white",
                shape: "chess-rook-black"
            }
        },
        {
            fields: [
                ["A", "2"], ["C", "7"]
            ],
            style: {
                color: "white",
                shape: "chess-knight-black"
            }
        },
        {
            fields: [
                ["A", "3"], ["B", "6"], ["D", "8"]
            ],
            style: {
                color: "white",
                shape: "chess-bishop-black"
            }
        },
        {
            fields: [
                ["A", "4"]
            ],
            style: {
                color: "white",
                shape: "chess-queen-black"
            }
        },
        {
            fields: [
                ["A", "5"]
            ],
            style: {
                color: "white",
                shape: "chess-king-black"
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
