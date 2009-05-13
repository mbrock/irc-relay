package se.htns.irc


import actors.Actor

class IRCServerLineWriter (socket: LineSocket) extends Actor {
  def act () {
    while (true) {
      receive {
        case message: IRCMessage =>
          socket.writeLine(message.toIRCString)
      }
    }
  }
}