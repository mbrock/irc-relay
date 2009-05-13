package se.htns.irc


import scala.actors._

class IRCServerHandler (socket: LineSocket,
                        serverInfo: IRCServerInfo) extends Actor {
  val serverReader = new IRCServerLineReader(socket, this)
  val serverWriter = new IRCServerLineWriter(socket)

  def act () {
    serverReader.start
    serverWriter.start
    Actor.loop {
      react {
        case ReceivedIRCMessage(message) =>
          println("<= " + serverInfo + ": " + message)
          handleMessage(message)
        case SendIRCMessage(message) =>
          println("=> " + serverInfo + ": " + message)
          serverWriter ! message
      }
    }
  }

  def handleMessage (message: IRCMessage): Unit = {
    message.command match {
      case "PING" => send("PONG", List(), message.text)
      case _ => ()
    }
  }

  def send (command: String, params: List[String],
            text: Option[String]): Unit =
    this ! SendIRCMessage(IRCMessage(None, command, params, text))

  def send (command: String, params: List[String]): Unit =
    send(command, params, None)

  def send (command: String): Unit =
    send(command, List())
}