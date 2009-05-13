package se.htns.irc


import java.io._
import java.net.Socket
import utilities.UntilNull

abstract class LineSocket extends Iterable[String] {
  def readLine (): String

  def writeLine (line: String)

  def elements = new UntilNull(readLine)
}

class TCPLineSocket (socket: Socket) extends LineSocket {
  val reader = new BufferedReader(
    new InputStreamReader(socket.getInputStream))
  val writer = new DataOutputStream(socket.getOutputStream)

  override def readLine = reader.readLine

  override def writeLine (s: String) = {
    writer.writeBytes(s + "\r\n")
  }

  def this (host: String, port: Int) = this (new Socket(host, port))
}