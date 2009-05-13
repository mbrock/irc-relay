package se.htns.irc


import util.matching.Regex
import util.parsing.combinator.RegexParsers

object IRCParser extends RegexParsers {
  override val whiteSpace = "".r // don't mess with spaces, scala!

  val spaces = """\s*""".r
  val command = ("""\w+""".r | """\d\d\d""".r) <~ spaces
  val prefix = ":" ~> """[^ ]+""".r <~ spaces
  val params = repsep("""[^: ][^ ]*""".r, spaces) <~ spaces
  val trailing = ":" ~> """.*""".r

  val message =
  opt(prefix) ~ command ~ params ~ opt(trailing) ^^
          {
            case prefix ~ command ~ params ~ trailing =>
              IRCMessage(prefix, command, params, trailing)
          }

  def parseMessage (string: String) =
    parse(message, string) match {
      case Success(m, _) => Some(m)
      case _ => None
    }
}

case class IRCMessage (prefix: Option[String],
                       command: String,
                       params: List[String],
                       text: Option[String]) {
  val toIRCString = prefix.map(":" + _ + " ").getOrElse("") + command +
          " " + params.mkString(" ") + text.map(" :" + _).getOrElse("")
}

case class ReceivedIRCMessage (message: IRCMessage)
case class SendIRCMessage (message: IRCMessage)