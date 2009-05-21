
package se.htns.utilities

object CharsetGuesser {
  import com.glaforge.i18n.io.CharsetToolkit

  def decode (string: String): String = {
    val toolkit = new CharsetToolkit(string.getBytes)
    val charset = toolkit.guessEncoding.name
    new String(string.getBytes, charset)
  }
}
