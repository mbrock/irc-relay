package se.htns.irc.specification


import org.specs.matcher.Matcher
import org.specs.Specification

object IRCParserSpec extends Specification {
  "IRC message parser" should {
    case class parseTo (expected: IRCMessage) extends Matcher[String]() {
      def apply (s: => String) =
        IRCParser parseMessage s match {
          case None => (false, null, "``" + s + "'' didn't parse")
          case Some(m) => (m == expected, "``" + s + "'' parsed right",
                  "``" + s + "'' parsed to " + m + " instead of " + expected)
        }
    }

    "parse a trivial numeric message" in {
      "123" must parseTo(IRCMessage(None, "123", List(), None))
    }

    "parse a full numeric message" in {
      ":foo 123 bar baz :suffix stuff" must parseTo(IRCMessage(
        Some("foo"), "123", List("bar", "baz"), Some("suffix stuff")))
    }

    "parse a symbolic message" in {
      ":foo TOPIC #test :new topic" must parseTo(IRCMessage(
        Some("foo"), "TOPIC", List("#test"), Some("new topic")))
    }

    "parse a ping message" in {
      "PING :123" must parseTo(IRCMessage(None, "PING", List(), Some("123")))
    }
  }
}