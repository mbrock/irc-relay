
require 'rubygems'
require 'eventmachine'
require 'evma_httpserver'
require 'em-websocket'

require 'em-ruby-irc'

def returning(x)
  yield; x
end

class MyHttpServer < EM::Connection
  include EM::HttpServer

  def post_init
    super
    no_environment_strings
  end
  
  def process_http_request
    response = EM::DelegatedHttpResponse.new(self)
    response.status = 200
    response.content_type 'text/html'
    response.content = File.read "index.html"
    response.send_response
  end
end

class IRCMessage
  attr_accessor :prefix, :command, :params, :text

  def initialize(prefix, command, params, text)
    @prefix, @command, @params, @text = prefix, command, params, text
  end
  
  def encode
    p [command, @params]
    if @params.respond_to? :join
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
    s = "" if s == nil
    puts 'Trying to decode: ' + s
    if s =~ /^(?:[:@](\S+) )?(\S+)(?: ((?:[^:\s]\S* ?)*))?(?: (.*))?$/
      prefix, cmd, @params, text = $1, $2, $3, $4
      prefix = nil if prefix.nil?
      text = nil if text.nil?
      new(prefix, cmd, @params, text)
    else
      nil
    end
  end
end

class IRCServerConnection < EM::Connection
  include EM::Protocols::LineText2

  def initialize(args)
    @nick = args[:nick]
    @host = args[:host]
    @port = args[:port]
    @channel = args[:channel]
    super
  end

  def connection_completed
    @channel.push("Connected to #@host:#@port.")
    puts "connected to " + @host + ":" + @port.to_s
    send_command(nil, 'USER', [@nick, @nick, '0', '0'], @nick)
    send_command(nil, 'NICK', @nick)
  end

  def send_command(prefix, command, params, text = nil)
    s = IRCMessage.new(prefix, command, params, text).encode
    puts s
    send_data(s + "\n")
  end

  def join_channel(channel)
    send_command(nil, "JOIN", channel)
  end

  def part_channel(channel)
    send_command(nil, "PART", channel)
  end

  def send_message(receiver, message)
    send_command(nil, "PRIVMSG", receiver, message)
  end
  
  def receive_line(line)
    message = IRCMessage.decode(line)
    p message
    if message == nil
      nil
    elsif message.command == "PING"
      send_command(nil, "PONG", message.text)
    elsif message.command == "PRIVMSG"
      @channel.push("&lt;#{message.prefix}&gt; #{message.text}<br>")
    else
      @channel.push(message.inspect)
    end
    message
  end
end

EM.run{
  channel = EM::Channel.new

  EM.start_server "0.0.0.0", 8080, MyHttpServer

  EM::WebSocket.start(:host => "0.0.0.0", :port => 1337, :debug => true) do |ws|
    ws.onopen {
      ws.send "Hello Client!"
      channel.subscribe { |msg| ws.send(msg + "<br>") }
    }
    ws.onmessage { |msg| ws.send "Pong: #{msg}" }
    ws.onclose   { puts "WebSocket closed" }
    ws.onerror   { |e| puts "Error: #{e.message}" }
  end

  EM.connect("irc.freenode.net", 6667, IRCServerConnection, :nick => "kokokaka", :host => "irc.freenode.net", :port => 6667, :channel => channel)
}


