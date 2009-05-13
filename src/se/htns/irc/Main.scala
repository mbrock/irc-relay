package se.htns.irc


object Main {
  def main (args: Array[String]) {
    val socket = new TCPLineSocket("irc.no.quakenet.org", 6667)
    val user = IRCServerUserSettings("foobar", "Foo Bar")
    val info = IRCServerInfo(0)
    val serverHandler = new IRCServerHandler(socket, info)
    serverHandler.start
    serverHandler.send("USER", List(user.username, "0", "0"),
      Some(user.realname))
    serverHandler.send("NICK", List("snubbelfarfar"))

    while (true) {
      Thread.sleep(500)
    }
  }
}