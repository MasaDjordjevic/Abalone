var smackjack = {
    echo: function (data, callback, errorHandler) { },  
    exampleChess: function (data, callback, errorHandler) { },
    exampleHexaChess: function (data, callback, errorHandler) { },
    exampleGomokuEastern: function (data, callback, errorHandler) { },
    exampleGomokuWestern: function (data, callback, errorHandler) { },
    exampleXo: function (data, callback, errorHandler) { },
    odigrajPotez: function (data, callback, errorHandler) { },

};
(function() {
    var httpFactory = null;
    var httpFactories = [function() {
        return new XMLHttpRequest();
    }, function() {
        return new ActiveXObject("Msxml2.XMLHTTP");
    }, function() {
        return new ActiveXObject("Microsoft.XMLHTTP");
    }];
    function httpNewRequest() {
        if (httpFactory) {
            return httpFactory();
        } else {
            var request = null;
            for (var i = 0, l = httpFactories.length, factory = httpFactories[i]; !(request !== null || i >= l); i += 1, factory = httpFactories[i]) {
                try {
                    request = factory();
                } catch (e) {
                };
            };
            if (request === null) {
                httpFactory = function() {
                    throw new Error("XMLHttpRequest not supported");
                };
                return httpFactory();
            } else {
                return request;
            };
        };
    };
    function identity(x) {
        return x;
    };
    function responseXml(request) {
        return request.responseXML;
    };
    function responseText(request) {
        return request.responseText;
    };
    function responseXmlText(request) {
        var result = "";
        var n = request.responseXML.firstChild;
        if (n) {
            n = n.firstChild;
            if (n) {
                result = n.nodeValue;
            };
        };
        return result;
    };
    function responseJson(request) {
        return JSON.parse(request.responseText);
    };



    function fetchUri(uri, callback, method, body, errorHandler, process, port = "8081") {
        if (method === undefined) {
            method = "GET";
        };
        uri = "http://localhost:" + port + uri;
        //uri = 	"http://localhost:8080/repl-api/ECHO/?arg0=%22%2B%201%202%22"
        var request = httpNewRequest();
        if (!request) {
            console.log("Browser couldn\'t make a request object.");
        };
        request.open(method, uri, true);
        request.onreadystatechange = function() {
            if (4 == request.readyState) {
                if (request.status >= 200 && request.status < 300 || request.status == 304) {
                    if (callback != null) {
                        callback(process(request));
                    };
                } else {
                    if (errorHandler == null) {
                        console.log("Error while fetching URI " + uri + " " + request.status + " " + request.statusText);
                    } else {
                        errorHandler(request);
                    };
                };
            };
            return null;
        };
        if (method == "POST") {
            request.setRequestHeader("Content-Type", "application/x-www-form-urlencoded");
        };

        request.send(body);
        return null;
    };
    function ajaxEncodeArgs(args) {
        var s = "";
        for (var i = 0; i < args.length; i += 1) {
            if (i > 0) {
                s += "&";
            };
            s += "arg" + i + "=" + encodeURIComponent(JSON.stringify(args[i]));
        };
        return s;
    };
    function ajaxCall(func, args, method, callback, errorHandler, process, port = "8081") {
        if (method === undefined) {
            method = "GET";
        };
        var uri = "/repl-api" + "/" + encodeURIComponent(func) + "/";
        var ajaxArgs = ajaxEncodeArgs(args);
        var body = null;
        if (method == "GET" && args.length > 0) {
            uri += "?" + ajaxArgs;
        };
        if (method == "POST") {
            body = ajaxArgs;
        };

        return fetchUri(uri, callback, method, body, errorHandler, process, port);
    };
    ;
    function echo(data, callback, errorHandler = null) {
        return ajaxCall("ECHO", [data], "GET", callback, errorHandler, responseText);
    }
    ;
    smackjack.echo = echo;



    function helloWorld(data, callback, errorHandler = null) {
        return ajaxCall("HELLO-WORLD", [data], "GET", callback, errorHandler, responseText);
    }
    ;
    smackjack.helloWorld = helloWorld;

    function test(data, callback, errorHandler = null) {
        return ajaxCall("TEST", [data], "GET", callback, errorHandler, responseText);
    }
    ;
    smackjack.test = test;

    function prvi(data, callback, errorHandler = null) {
        return ajaxCall("PRVI", [data], "GET", callback, errorHandler, responseText);
    }
    ;
    smackjack.prvi = prvi;

    function drugi(data, callback, errorHandler = null) {
        return ajaxCall("DRUGI", [data], "GET", callback, errorHandler, responseText);
    }
    ;
    smackjack.drugi = drugi;

    function exampleChess(data, callback, errorHandler = null) {
        return ajaxCall("EXAMPLE-CHESS", [data], "GET", callback, errorHandler, responseText);
    }
    ;
    smackjack.exampleChess = exampleChess;
    function exampleHexaChess(data, callback, errorHandler = null) {
        return ajaxCall("EXAMPLE-HEXA-CHESS", [data], "GET", callback, errorHandler, responseText);
    }
    ;
    smackjack.exampleHexaChess = exampleHexaChess;
    function exampleGomokuEastern(data, callback, errorHandler = null) {
        return ajaxCall("EXAMPLE-GOMOKU-EASTERN", [data], "GET", callback, errorHandler, responseText);
    }
    ;
    smackjack.exampleGomokuEastern = exampleGomokuEastern;
    function exampleGomokuWestern(data, callback, errorHandler = null) {
        return ajaxCall("EXAMPLE-GOMOKU-WESTERN", [data], "GET", callback, errorHandler, responseText);
    }
    ;
    smackjack.exampleGomokuWestern = exampleGomokuWestern;

    function exampleXo(data, callback, errorHandler = null) {
        return ajaxCall("EXAMPLE-XO", [data], "GET", callback, errorHandler, responseText);
    }
    ;
    smackjack.exampleXo = exampleXo;


    function reset(data, callback, errorHandler = null, port = "8081") {
        return ajaxCall("RESET", [data], "GET", callback, errorHandler, responseText, port);
    }
    ;
    smackjack.reset = reset;

    function odigrajPotez(data, callback, errorHandler = null, port = "8081") {
        return ajaxCall("ODIGRAJ-POTEZ", [data], "POST", callback, errorHandler, responseText, port);
    }
    ;
    smackjack.odigrajPotez = odigrajPotez;
    return null;
})();
