# A prototype of a logger!
class LogDatabase
  def log!(server_name, message)
    @message_no ||= 0
    @message_no += 1
    print "{#@message_no}"
    print server_name + ": "
    print message.prefix  + " " unless message.prefix.nil?
    print message.command + " " unless message.command.nil?
    print message.params  + " " unless message.params.nil?
    print message.text    + " " unless message.text.nil?
    print "\n"
    @message_no
  end
end
