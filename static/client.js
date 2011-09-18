// The websocket server. 
var ws = null;
var username = null;

// Outputs the message in the message div
function outputMessage(string) {
    $("#messages").append("<div class=\"line\">"+string+"</div>");
}

function stringToCommand(string){
    if(string.charAt(0) == '/'){
        // Okay, this is a command
        if(matches = string.match("/connect\\s([\\w\\.]+)\\s(\\d+)")){
            return JSON.stringify({command:"connect", hostname: matches[1], port: matches[2]});
        }
    }else{
        // If not command then display as chat-line
        return JSON.stringify({"command":"send", "server":"irc.freenode.net", "message":{"command":"PRIVMSG", "text":msg, "params":["salkin"]}});
    }

}

// Init websocket server on page load.
$(document).ready( function() {
    // Starts the socket
    var Socket = "MozWebSocket" in window ? MozWebSocket : WebSocket;
    ws = new Socket("ws://localhost:1337/");
        
    // The callbacks
    ws.onmessage = function(evt) { 
        data = JSON.parse(evt.data);
        if(data.command == "connect"){
            outputMessage("Connecting to " + data.hostname + ":" + data.port);
        }else {
            outputMessage(evt.data);
        }
    };
    ws.onclose = function() { outputMessage("Socket closed"); };
    ws.onopen = function() {
        outputMessage("Connected to backend.");
        
//        ws.send(JSON.stringify({command:"send", server:"irc.freenode.net", 
//                                message:{command:"USER", params:["mamaoeu","mamaoeu",0,0], text:"mamaoeu"}}));

    }
    
    // 
    $("#input_form").submit(function() {
      // Send the text from the input field on the webserver
      var command = stringToCommand($("#input_text").val());
        if(command){
            ws.send(command);
        }

    // Push current input to history stack
    
    // Clear the field
    $("#input_text").val("");

    // Return false so the webpage does not reload because of form submit
    return false;
  });

});
