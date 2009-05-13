package se.htns.irc


import actors.Actor
import java.io.{InputStreamReader, BufferedReader}
import java.net.Socket

class IRCServerLineReader (socket: LineSocket,
                           actor: Actor) extends Thread {
  override def run {
    for (line <- socket) {
      IRCParser parseMessage line match {
        case Some(message) => actor ! ReceivedIRCMessage(message)
        case None => println("broken message: " + line)
      }
    }

    println("connection closed")
  }
}