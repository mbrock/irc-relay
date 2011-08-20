# -*- coding: utf-8 -*-
require 'gserver'

class IRCServerConnection
  def initialize(hostname, port)
    Thread.new do
      @socket = TCPSocket.new(hostname, port)
      loop do
        handle(@socket.gets)
      end
    end
  end
  
  def handle(line)
    puts line
  end
end

class IRCRelayUser
  def initialize(username)
    @connections = []
  end

  def connect(hostname, port)
    @connections << IRCServerConnection.new(hostname, port)
  end
end

class IRCRelayServer < GServer
  def initialize(port=4002)
    super(port, 0)
  end

  def serve(io)
    @user = IRCRelayUser.new(io.gets)
    if io.gets =~ /^c (.*) (.*)$/
      @user.connect $1, $2
    end
    io.puts "Välkommen till IRC Relay Server!"
    io.puts "Hoppas du trivs."
  end
end

relay_server = IRCRelayServer.new
relay_server.audit = true
relay_server.start.join
