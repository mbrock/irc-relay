package se.htns.io

import se.htns.utilities.Startable

import java.net.{Socket, ServerSocket}

trait HasServerSocket {
  val serverSocket: ServerSocket
}

trait TCPAccepter extends Startable with HasServerSocket {
  def handleNewSocket (socket: Socket): Unit

  object accepterThread extends Thread {
    override def run = {
      while (true) {
        val socket = serverSocket.accept
        handleNewSocket(socket)
      }
    }
  }

  override def start {
    super.start
    accepterThread.start
  }
}
