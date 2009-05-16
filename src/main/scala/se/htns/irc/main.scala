package se.htns.irc


object Test {
  def main (args: Array[String]) {
    val socket = new TCPLineSocket("irc.no.quakenet.org", 6667)
    val info = IRCServerInfo(0)
    val serverHandler = new IRCServerHandler(socket, info)
    serverHandler.sendIRCServerMessage(
      IRCMessage(None, "USER", List("my username", "0", "0"),
        Some("my realname")))
    serverHandler.sendIRCServerMessage(
      IRCMessage(None, "NICK", List("snubbelfarfar"), None))
    serverHandler.waitUntilDone
  }
}
