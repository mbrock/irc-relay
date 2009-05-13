package se.htns.irc.specification


import collection.jcl.Buffer
import collection.mutable.ArrayBuffer
import org.specs.mock.Mockito
import org.specs.Specification

object IRCServerHandlerSpec extends Specification with Mockito {
  class LoggingLineSocket extends LineSocket {
    var lines: List[String] = List()

    override def readLine = null

    override def writeLine (line: String) = (lines = lines + line)
  }

  "IRC server handler" should {
    "be able to write a USER message" in {
      val socket = new LoggingLineSocket
      val handler = new IRCServerHandler(socket, IRCServerInfo(0))
      handler.start
      handler.send("USER", List("foobar", "0", "0"), Some("Foo Bar"))
      println(socket.lines)
    }
  }
}