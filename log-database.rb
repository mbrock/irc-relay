class LogDatabase
  def initialize
    @message_no = 0
  end

  def log! server, message
    puts "Logging #{server} #{message}"
    @message_no += 1
  end
end
