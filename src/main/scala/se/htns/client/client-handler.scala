
package se.htns.client

import se.htns.lineio._

import java.net._

import scala.collection.mutable._

import com.twitter.commons.Json

trait ClientBroadcaster {
  def broadcastClientMessage (message: Any) : Unit
}

trait ClientLogic {
  def sendClientMessage (message: Any) : Unit

  def handleClientMessage (message: Any) : Unit = {
    println("=> client: " + message)
  }
}

class ClientHandler (val lineSocket: LineSocket)
    extends ClientLogic with HasLineReadingThread with HasLineWritingThread
{
  def handleLine (line: String): Unit =
    handleClientMessage(Json parse line)

  def handleEOF : Unit =
    println("client EOF")

  def sendClientMessage (message: Any) : Unit =
    writeLine(Json.build(message).toString)
}

class ClientAccepter (serverSocket: ServerSocket) 
    extends Thread with ClientBroadcaster {
  val clients : Buffer[ClientHandler] = new ArrayBuffer

  def broadcastClientMessage (message: Any) : Unit = {
    clients synchronized { clients foreach (_.sendClientMessage(message)) }
  }

  override def run = {
    while (true) {
      val socket = serverSocket.accept
      clients synchronized {
        clients += new ClientHandler(new TCPLineSocket(socket))
      }
    }
  }
}
