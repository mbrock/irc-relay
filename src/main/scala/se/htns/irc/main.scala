package se.htns.irc

import se.htns.client._

import java.net._

object Test {
  def main (args: Array[String]) {
    val serverSocket = new ServerSocket(7776)
    val clientAccepter = new ClientAccepter(serverSocket)
    clientAccepter.start

    val socket = new TCPLineSocket("irc.no.quakenet.org", 6667)
    val info = IRCServerInfo(0)
    val serverHandler = new IRCServerHandler(socket, info, clientAccepter)
    serverHandler.sendIRCServerMessage(
      IRCMessage(None, "USER", List("my username", "0", "0"),
        Some("my realname")))
    serverHandler.sendIRCServerMessage(
      IRCMessage(None, "NICK", List("snubbelfarfar"), None))
    serverHandler.waitUntilDone
  }
}
