package se.htns.irc


import java.io.{InputStreamReader, BufferedReader}
import java.net.Socket
import utilities.UntilNull

abstract class LineSocket extends Iterable[String] {
  def readLine (): String

  def elements = new UntilNull(readLine)
}

class TCPLineSocket (socket: Socket) extends LineSocket {
  val reader = new BufferedReader(new InputStreamReader(socket.getInputStream))

  override def readLine = reader.readLine

  def this (host: String, port: Int) = this (new Socket(host, port))
}