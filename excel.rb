
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

class BackendConnection < EM::Connection
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

EM.run {
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

  EM.connect("localhost", 1338, BackendConnection)
}


