package se.htns.irc

import se.htns.lineio._

import scala.actors._
import utilities.BinarySemaphore

class IRCServerHandler (val lineSocket: LineSocket,
                        val serverInfo: IRCServerInfo)
    extends IRCServerLogic
       with HasLineWritingThread
       with HasLineReadingThread {

  def handleLine (line: String) : Unit = {
    IRCParser parseMessage line match {
      case Some(message) => handleIRCServerMessage(message)
      case None => println("!! " + serverInfo + " crazy: " + line)
    }
  }

  def handleEOF : Unit = handleIRCServerEOF

  def sendIRCServerMessage (message: IRCMessage) : Unit = {
    println("=> " + serverInfo + ": " + message)
    writeLine(message.toIRCString)
  }
}
