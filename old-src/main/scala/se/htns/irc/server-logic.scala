package se.htns.irc

import se.htns.client.ClientBroadcaster

import utilities.BinarySemaphore

case class IRCServerInfo (id: String, hostname: String, port: Int) {
  def jsonize = id
}

case class IRCUserInfo (nickname: String, username: String, realname: String)

trait IRCServerLogic {
  val serverInfo: IRCServerInfo
  val userInfo: IRCUserInfo
  val clientBroadcaster: ClientBroadcaster

  def login: Unit = {
    sendIRCServerMessage(
      IRCMessage(None, "USER", 
                 List(userInfo.username, "0", "0"),
                 Some(userInfo.realname)))
    sendIRCServerMessage(
      IRCMessage(None, "NICK", List(userInfo.nickname), None))
  }

  def broadcastClientMessage (message: Any): Unit =
    clientBroadcaster broadcastClientMessage message

  def sendIRCServerMessage (message: IRCMessage): Unit

  def handleIRCServerMessage (message: IRCMessage) : Unit = {
    println("<= " + serverInfo + ": " + message)
    message match {
      case IRCMessage(_, "PING", _, text) =>
        sendIRCServerMessage(IRCMessage(None, "PONG", List(), text))
      case IRCMessage(prefix, command, params, text) =>
        var map = Map("server" -> serverInfo.jsonize,
                      "command" -> command,
                      "params" -> params)
        prefix match {
          case Some(x) => map += (("prefix", x))
          case _ => ()
        }
        text match {
          case Some(x) => map += (("text", x))
          case _ => ()
        }
        broadcastClientMessage(map)
    }
  }

  private object doneSemaphore extends BinarySemaphore

  def handleIRCServerEOF = {
    println("!! " + serverInfo + ": eof")
    doneSemaphore.activate
  }

  def waitUntilDone = doneSemaphore.block
}
