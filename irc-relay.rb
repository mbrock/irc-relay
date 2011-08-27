# -*- coding: utf-8 -*-
require 'gserver'

Thread.abort_on_exception = true

def returning(x)
  yield; x
end

class IRCMessage
  attr_accessor :prefix, :command, :params, :text

  def initialize(prefix, command, params, text)
    @prefix, @command, @params, @text = prefix, command, params, text
  end
  
  def encode
    p [command, @params]
    if @params.respond_to? :join
      puts 'wat?'
      @params = @params.join(' ')
    end
    p @params
    returning(s = "") do
      s << ':' + prefix + ' ' unless prefix.nil?
      s << command
      s << ' ' + @params unless @params.empty?
      s << ' :' + text unless text.nil?
    end
  end

  def self.decode(s)
    puts 'Trying to decode: ' + s
    if s =~ /^(?:[:@](\S+) )?(\S+)(?: ((?:[^:\s]\S* ?)*))?(?:(?:.*))?$/
      prefix, cmd, @params, text = $1, $2, $3, $4
      prefix = nil if prefix.nil?
      text = nil if text.nil?
      puts 'hmm'
      new(prefix, cmd, @params, text)
    else
      puts 'UH OH'
    end
  end
end

class IRCServerConnection
  def initialize(hostname, port, nickname, username, realname)
    Thread.new do
      @socket = TCPSocket.new(hostname, port)
      puts "connected to #{hostname}"
      send_command(nil, 'USER', [username, realname, '0', '0'], realname)
      send_command(nil, 'NICK', nickname)
      loop do
        handle(@socket.gets)
      end
    end
  end

  def send_command(prefix, command, params, text = nil)
    s = IRCMessage.new(prefix, command, params, text).encode
    puts s
    @socket.puts s
  end
  
  def handle(line)
    p IRCMessage.decode(line)
  end
end

class IRCRelayUser
  def initialize(username)
    @connections = []
  end

  def connect(hostname, port, nickname, username, realname)
    @connections << IRCServerConnection.new(hostname, port,
                                            nickname, username, realname)
  end
end

class IRCRelayServer < GServer
  def initialize(port=4002)
    super(port, 0)
  end

  def serve(io)
    name = io.gets.gsub(/\n/, "")
    @user = IRCRelayUser.new(name)
    if io.gets =~ /^c (.*) (.*)$/
      @user.connect $1, $2, name, name, name
    else
      io.puts "WTF?"
    end
    io.puts "Välkommen till IRC Relay Server!"
    io.puts "Hoppas du trivs."
  end
end

relay_server = IRCRelayServer.new
relay_server.audit = true
relay_server.debug = true
relay_server.start.join
