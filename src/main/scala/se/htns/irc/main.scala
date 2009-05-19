package se.htns.irc

import se.htns.client._

import java.net._

object Test {
  def main (args: Array[String]) {
    val multiplexer = new Multiplexer(
      new ServerSocket(7776),
      IRCUserInfo("mirakel", "mirakel", "mirakel"))
    multiplexer.start
    
    while (true)
      Thread sleep 10000
  }
}
