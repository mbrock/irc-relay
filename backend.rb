require 'rubygems'
require 'json'
require 'eventmachine'
require 'log-database'

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
    if s =~ /^(?:[:@](\S+) )?(\S+)(?: ((?:[^:\s]\S* ?)*))?(?: :?(.*))?$/
      prefix, cmd, @params, text = $1, $2, $3, $4
      prefix = nil if prefix.nil?
      text = nil if text.nil?
      new(prefix, cmd, @params, text)
    else
      nil
    end
  end

  def to_hash
    { :prefix  => prefix,
      :command => command,
      :params  => params,
      :text    => text }
  end
end

class Backend
  def initialize(args)
    @channel = args[:channel]
    @database = args[:database]
  end

  def connect(hostname, port)
    EM::connect(hostname, port, IRCServerConnection,
                :backend  => self,
                :hostname => hostname)
  end

  def receive_message(server_name, message)
    message_no = @database.log!(server_name, message)
    @channel.push({ :message_no  => message_no,
                    :server_name => server_name,
                    :message     => message.to_hash })
  end
end

class RelayConnection < EM::Connection
  include EM::Protocols::LineText2

  def initialize(args)
    @backend = args[:backend]
    
    args[:channel].subscribe do |message|
      send_data(JSON.generate(message) + "\n")
      super
    end

    def receive_line(line)
      message = JSON.load(line)
      case message['command']
      when 'connect'
        @backend.connect message['hostname'], message['port']
      when 'send'
        @backend.send message['server'], message['message']
      end
    end
  end
end

class IRCServerConnection < EM::Connection
  include EM::Protocols::LineText2

  def initialize(args)
    @hostname = args[:hostname]
    @backend = args[:backend]
    super
  end

  def server_name ; @hostname end

  def connection_completed
    puts "Connected to " + @hostname
  end

  def send_message(prefix, command, params, text = nil)
    s = IRCMessage.new(prefix, command, params, text).encode
    puts "Sending to #{server_name}: #{s}"
    send_data(s.encode + "\n")
  end

  def receive_line(line)
    message = IRCMessage.decode(line)
    if message == nil
      puts "Crazy message from #{server_name}: #{line}"
    elsif message.command == "PING"
      send_message(nil, "PONG", message.text)
    else
      @backend.receive_message(server_name, message)
    end
  end
end

EM.run {
  channel = EM::Channel.new
  database = LogDatabase.new
  backend = Backend.new(:channel => channel, :database => database)

  EM.start_server("0.0.0.0", 1338, RelayConnection,
                  :backend => backend,
                  :channel => channel)
}
