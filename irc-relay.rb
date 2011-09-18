
require 'rubygems'
require 'eventmachine'
require 'evma_httpserver'
require 'em-websocket'
require 'json'

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
    @channel = args[:messages_to_client]
    args[:messages_to_backend].subscribe do |message|
      puts "backend_connection got: #{message}"
      send_data(message + "\n")
    end
    super
  end
  
  def connection_completed
    puts "connected to the backend"
  end

  def receive_line(line)
    puts line
    msg = JSON.load(line)
    message = msg['message']
    if message == nil
      nil
    else
      @channel.push(message)
    end
  end
end

class RelayServer < EM::Connection
  include EM::Protocols::LineText2

  def initialize(args)
    @messages_to_backend = args[:messages_to_backend]

    args[:messages_to_client].subscribe do |message|
      send_data(JSON.generate(message) + "\n")
    end
    super
  end

  def receive_line(line)
    puts "relay_server got: #{line}"
    @messages_to_backend.push(line)
  end
end

EM.run {
  messages_to_client = EM::Channel.new
  messages_to_backend = EM::Channel.new

  EM.start_server '0.0.0.0', 8080, MyHttpServer

  EM::WebSocket.start(:host => "0.0.0.0", :port => 1337, :debug => true) do |ws|
    ws.onopen {
      messages_to_client.subscribe { |msg| ws.send(msg) }
    }
    ws.onmessage { |msg| messages_to_client.push(msg) }
    ws.onclose   { puts "WebSocket closed" }
    ws.onerror   { |e| puts "Error: #{e.message}" }
  end

  EM.connect("localhost", 1338, BackendConnection,
             :messages_to_backend => messages_to_backend,
             :messages_to_client  => messages_to_client)
  EM.start_server('0.0.0.0', 1339, RelayServer,
                  :messages_to_backend => messages_to_backend,
                  :messages_to_client  => messages_to_client)
}
