
package se.htns.client

import se.htns.irc._

import java.net._

import scala.collection.immutable.Set

import com.twitter.commons.Json

trait ClientBroadcaster {
  def broadcastClientMessage (message: Any) : Unit
}

class Multiplexer (val serverSocket: ServerSocket,
                   val userInfo: IRCUserInfo) extends ClientBroadcaster {
  var servers: Set[IRCServerHandler] = Set()
  var clients: Set[ClientHandler] = Set()
  val multiplexer: Multiplexer = this

  def addServerConnection (info: IRCServerInfo): Unit = {
    val socket = new TCPLineSocket(info.hostname, info.port)
    val handler = new IRCServerHandler(socket, info, userInfo, this)
    handler.login
    this synchronized { servers += handler }
  }

  def broadcastClientMessage (message: Any): Unit = {
    this synchronized { clients foreach (_.sendClientMessage(message)) }
  }

  object accepterThread extends Thread {
    override def run = {
      while (true) {
        val socket = serverSocket.accept
        this synchronized {
          clients += new ClientHandler(new TCPLineSocket(socket), multiplexer)
        }
      }
    }
  }

  accepterThread.start
}

trait ClientLogic {
  val multiplexer: Multiplexer

  def sendClientMessage (message: Any) : Unit

  def handleClientMessage (message: Any) : Unit = {
    message match {
      case map: Map[Any, Any] =>
        (map get "command", map get "data") match {
          case (Some(command: String), Some(data: Map[String, Any])) =>
            handleClientCommand(command, data)
          case _ =>
            println("=> non-command client message: " + message)
        }
      case _ =>
        println("=> crazy client message: " + message)
    }
  }

  def handleClientCommand (command: String, data: Map[String, Any]) : Unit = {
    command match {
      case "connect" =>
        handleConnectCommand(data)
      case _ =>
        println("unhandled command: " + command)
    }
  }

  def handleConnectCommand (data: Map[String, Any]) : Unit = {
    (data get "serverID", data get "hostname", data get "port") match {
      case (Some(id: String), Some(hostname: String), Some(port: Int)) =>
        multiplexer addServerConnection IRCServerInfo(id, hostname, port)
      case _ =>
        println("buggy connect message data: " + data)
    }
  }
}

class ClientHandler (val lineSocket: LineSocket,
                     val multiplexer: Multiplexer)
    extends ClientLogic with HasLineReadingThread with HasLineWritingThread
{
  def sendClientMessage (message: Any) = writeLine(Json build message toString)
  def handleLine (line: String)        = handleClientMessage(Json parse line)
  def handleEOF                        = println("client EOF")
}
