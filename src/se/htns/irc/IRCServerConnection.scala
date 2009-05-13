package se.htns.irc


import java.io.{InputStreamReader, BufferedReader}
import java.net.Socket

class IRCServerConnection (socket: LineSocket,
                           userSettings: IRCServerUserSettings) {
  def run {
    for (line <- socket) {
      IRCParser parseMessage line match {
        case Some(message) => println(message)
        case None => println("broken message: " + line)
      }
    }

    println("connection closed")
  }
}