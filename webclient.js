// The websocket server. 
var ws = null;

// Outputs the message in the message div
function outputMessage(string) {
    $("#messages").append("<div>"+string+"</div>");
}

// Init websocker server on page load.
$(document).ready( function() {
    // Starts the socket
    var Socket = "MozWebSocket" in window ? MozWebSocket : WebSocket;
    ws = new Socket("ws://localhost:1337/");

    // The callbacks
    ws.onmessage = function(evt) { 
        data = JSON.parse(evt.data);
        if(data.command == "connect"){
            outputMessage("Connecting to " + data.hostname + ":" + data.port);
        }else if(data.command == "send"){
            outputMessage("<< " + data.message.command + " " + data.message.params);
        }
    };
    ws.onclose = function() { outputMessage("Socket closed"); };
    ws.onopen = function() {
        outputMessage("Connected to backend.");
        ws.send(JSON.stringify({command:"connect", hostname:"irc.freenode.net", port:6667}));
        ws.send(JSON.stringify({command:"send", server:"irc.freenode.net", 
                                message:{command:"USER", params:["mamaoeu","mamaoeu",0,0], text:"mamaoeu"}}));

    }
});
$("#input_form").submit(function() {
    // Send the text from the input field on the webserver
    var msg = $("#input").val();
    var command = {"command":"send",
                   "server":"irc.freenode.net",
                   "message":{"command":"PRIVMSG",
                              "text":msg,
                              "params":["salkin"]}
                  }
    ws.send(JSON.stringify(command));
    
    // Clear the field
    $("#input").val("");

    // Return false so the webpage does not reload because of form submit
    return false;
});
