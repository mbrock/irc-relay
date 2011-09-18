
require 'rubygems'
require 'eventmachine'
require 'evma_httpserver'
require 'em-websocket'
require 'json'

def returning(x)
  yield; x
end

# The web server. Puts index.html on port 8080
class MyHttpServer < EM::Connection
  include EM::HttpServer

  def post_init
    super
    no_environment_strings
  end
  
  def process_http_request
    response = EM::DelegatedHttpResponse.new(self)
    ## TODO: make this safe
    if true # @@allowed_requests.include? @http_request_uri
      if @http_request_uri == '/'
        path = '/index.html'
      elsif
        path = @http_request_uri
      end

      response.status = 200
      case path
      when /\.html$/
        response.content_type 'text/html'
      when /\.css$/
        response.content_type 'text/css'
      when /\.js$/
        response.content_type 'text/javascript'
      else
        response.content_type 'text/plain'
      end
      response.content = File.read(File.join('static', path))
    else
      response.status = 404
    end
    response.send_response
  end
end

# The Backend Connection
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

class WebSocketClientConnection
  def initialize(args)
    @host, @port, @debug = args[:host], args[:port], args[:debug]
    @messages_to_backend = args[:messages_to_backend]
    @messages_to_client  = args[:messages_to_client]
  end

  def start()
    puts("start from websocketclientconnection")
    EM::WebSocket.start(:host  => @host,
                        :port  => @port,
                        :debug => @debug) do |ws|
      ws.onopen do
        puts("onopen")
        @messages_to_client.subscribe do |msg| 
          ws.send(JSON.generate(msg) + "\n")
        end
      end
    
      ws.onmessage do |msg| 
        puts("onmessage " + msg)
        @messages_to_backend.push(msg) 
      end

      ws.onclose   { puts "WebSocket closed" }
      ws.onerror   { |e| puts "Error: #{e.message}" }
    end
  end
end

class SocketClientConnection < EM::Connection
  include EM::Protocols::LineText2

  def initialize(args)
    @messages_to_backend = args[:messages_to_backend]
    @messages_to_client = args[:messages_to_client]
    subscribe_to_client_messages
    super
  end

  def subscribe_to_client_messages
    @messages_to_client.subscribe do |message|
      send_data(JSON.generate(message) + "\n")
    end
  end

  def receive_line(line)
    puts "relay_server got: #{line}"
    @messages_to_backend.push(line)
  end
end

EM.run {
  # Create channels
  messages_to_client = EM::Channel.new
  messages_to_backend = EM::Channel.new

  # Start the webserver
  EM.start_server '0.0.0.0', 8080, MyHttpServer

  # Start web the socket.
  WebSocketClientConnection.new(:host                => '0.0.0.0', 
                                :port                => 1337, 
                                :debug               => true,
                                :messages_to_backend => messages_to_backend,
                                :messages_to_client  => messages_to_client).start

  # Connect to the backend
  EM.connect("localhost", 1338, BackendConnection,
             :messages_to_backend => messages_to_backend,
             :messages_to_client  => messages_to_client)

  # Start the server with a RelayServer object
  EM.start_server('0.0.0.0', 1339, SocketClientConnection,
                  :messages_to_backend => messages_to_backend,
                  :messages_to_client  => messages_to_client)
}
