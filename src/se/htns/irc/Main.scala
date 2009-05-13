package se.htns.irc


object Main {
  def main (args: Array[String]) {
    val socket = new TCPLineSocket("irc.freenode.net", 6667)
    val user = IRCServerUserSettings("foobar", "Foo Bar")
    val serverConnection = new IRCServerConnection(socket, user)
    serverConnection run
  }
}