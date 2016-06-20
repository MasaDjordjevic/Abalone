
var receivedData = null;

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
                [1, 0], [1, 1], [1, 2], [1, 3], [1, 4], [1, 5], [1, 6], [1, 7]
            ],
            style: {
                color: "black",
                shape: "chess-pawn-black"
            }
        },
        {
            // Beli pešaci
            fields: [
                [6, 0], [6, 1], [6, 2], [6, 3], [6, 4], [6, 5], [6, 6], [6, 7]
            ],
            style: {
                color: "white",
                shape: "chess-pawn-black"
            }
        },
        {
            // Crni topovi
            fields: [
                [0, 0], [0, 7]
            ],
            style: {
                color: "black",
                shape: "chess-rook-black"
            }
        },
        {
            fields: [
                [0, 1], [0, 6]
            ],
            style: {
                color: "black",
                shape: "chess-knight-black"
            }
        },
        {
            fields: [
                [0, 2], [0, 5]
            ],
            style: {
                color: "black",
                shape: "chess-bishop-black"
            }
        },
        {
            fields: [
                [0, 3]
            ],
            style: {
                color: "black",
                shape: "chess-queen-black"
            }
        },
        {
            fields: [
                [0, 4]
            ],
            style: {
                color: "black",
                shape: "chess-king-black"
            }
        },
        {
            // Beli topovi
            fields: [
                [7, 0], [7, 7]
            ],
            style: {
                color: "white",
                shape: "chess-rook-black"
            }
        },
        {
            fields: [
                [7, 1], [7, 6]
            ],
            style: {
                color: "white",
                shape: "chess-knight-black"
            }
        },
        {
            fields: [
                [7, 2], [7, 5]
            ],
            style: {
                color: "white",
                shape: "chess-bishop-black"
            }
        },
        {
            fields: [
                [7, 3]
            ],
            style: {
                color: "white",
                shape: "chess-queen-black"
            }
        },
        {
            fields: [
                [7, 4]
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

function helloWorld() {
    /*smackjack.helloWorld("masa test", log, null);

    var send = {"tip" : "white",
                "polja" : [1, 2, 9],
              "*radi*" : "idemooo!"};
    smackjack.test(JSON.stringify(send), prvi, null);

    debugger;*/
   //zoviPrvog();

   //smackjack.exampleChess(JSON.stringify(example.chess), parse, null);
   //smackjack.exampleChess(JSON.stringify(example.chess), parse, null);
   smackjack.reset(JSON.stringify(example.chess), parse, null);
   $("#example-chess").click(function(){
     smackjack.exampleChess("", parse, null);
   })
   $("#odigraj-potez").click(function(){
     smackjack.odigrajPotez(JSON.stringify(receivedData), parse, null);
   })
}

function log(data) {
  console.log(data);
}

function parse(data) {
  receivedData = (JSON.parse((JSON.parse(data))));
  console.log(receivedData);

  displayData(receivedData);
}

function zoviPrvog() {
  console.log("zovem prvog");
  smackjack.prvi(JSON.stringify(receivedData), prvi, null);
}

function prvi(data) {
  receivedData = (JSON.parse((JSON.parse(data))));

  console.log("dobio sam od prvog: ");
  console.log(receivedData);
  zoviDrugog();
}

function zoviDrugog() {
  console.log("zovem drugog");
  smackjack.drugi(JSON.stringify(receivedData), drugi, null);
}

function drugi(data) {
  receivedData = (JSON.parse((JSON.parse(data))));

  console.log("dobio sam od drugog: ");
  console.log(receivedData);
  //zoviPrvog();
}


window.onload = function () {
    helloWorld();
}
