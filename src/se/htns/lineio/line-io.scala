package se.htns.irc


import actors.{Channel, Actor}
import java.io._
import java.net.Socket
import utilities.UntilNull

abstract class LineSocket extends Iterable[String] {
  def readLine (): String
  def writeLine (line: String)
  def elements: Iterator[String] = new UntilNull(readLine)
}

class TCPLineSocket (socket: Socket) extends LineSocket {
  def this (host: String, port: Int) = this (new Socket(host, port))

  private val reader = new BufferedReader(new InputStreamReader(socket.getInputStream))
  private val writer = new DataOutputStream(socket.getOutputStream)

  override def readLine = reader.readLine
  override def writeLine (s: String) = writer.writeBytes(s + "\r\n")
}

trait HasLineSocket {
  val lineSocket: LineSocket
  def handleEOF: Unit
}

trait HasLineReadingThread extends HasLineSocket {
  def handleLine (line: String): Unit

  private object t extends Thread {
    override def run {
      lineSocket foreach handleLine
      handleEOF
    }
  }
  
  t start
}

trait HasLineWritingThread extends HasLineSocket {
  private val lineWritingActor: Actor = Actor.actor {
    Actor.loop { 
      Actor.receive {
        case line: String =>
          lineSocket writeLine line
      }
    }
  }

  def writeLine (line: String): Unit = lineWritingActor ! line
}
