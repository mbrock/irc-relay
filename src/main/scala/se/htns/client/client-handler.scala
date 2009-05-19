
package se.htns.client

import se.htns.irc._
import se.htns.io._

import java.net._

import scala.collection.immutable.Set

import com.twitter.commons.Json

trait ClientBroadcaster {
  def broadcastClientMessage (message: Any) : Unit
}

class Multiplexer (val serverSocket: ServerSocket,
                   userInfo: IRCUserInfo) 
    extends TCPAccepter with ClientBroadcaster {
  var servers: Set[IRCServerHandler] = Set()
  var clients: Set[ClientHandler] = Set()

  def addServerConnection (info: IRCServerInfo): Unit = {
    val socket = new TCPLineSocket(info.hostname, info.port)
    val handler = new IRCServerHandler(socket, info, userInfo, this)
    handler.login
    this synchronized { servers += handler }
  }

  def broadcastClientMessage (message: Any): Unit = {
    this synchronized { clients foreach (_.sendClientMessage(message)) }
  }

  def sendIRCServerMessage (serverID: String, message: IRCMessage): Unit = {
    servers find (_.serverID == serverID) match {
      case Some(server) =>
        server sendIRCServerMessage message
      case _ =>
        println("no such server '`(")
    }
  }

  def handleNewSocket (socket: Socket): Unit =
    this synchronized { clients += 
      new ClientHandler(new TCPLineSocket(socket), this) } 
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
      case "join" =>
        handleJoinCommand(data)
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

  def handleJoinCommand (data: Map[String, Any]) : Unit = {
    (data get "serverID", data get "channel") match {
      case (Some(id: String), Some(channel: String)) =>
        multiplexer sendIRCServerMessage (id,
          IRCMessage(None, "JOIN", List(channel), None))
      case _ =>
        println("buggy join message data: " + data)
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
