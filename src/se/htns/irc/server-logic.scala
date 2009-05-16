package se.htns.irc

import utilities.BinarySemaphore

case class IRCServerInfo (id: Int)

trait IRCServerLogic {
  val serverInfo: IRCServerInfo

  def sendIRCServerMessage (message: IRCMessage) : Unit

  def handleIRCServerMessage (message: IRCMessage) : Unit = {
    println("<= " + serverInfo + ": " + message)
    message match {
      case IRCMessage(_, "PING", _, text) =>
        sendIRCServerMessage(IRCMessage(None, "PONG", List(), text))
      case _ =>
        ()
    }
  }

  private object doneSemaphore extends BinarySemaphore

  def handleIRCServerEOF = {
    println("!! " + serverInfo + ": eof")
    doneSemaphore.activate
  }

  def waitUntilDone = doneSemaphore.block
}