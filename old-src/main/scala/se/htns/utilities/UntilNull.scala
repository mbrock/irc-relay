package se.htns.utilities


class UntilNull[T >: Null] (action: => T) extends Iterator[T] {
  var x: T = null

  override def hasNext: Boolean = {
    if (x == null)
      x = action
    x != null
  }

  override def next: T = {
    if (!hasNext)
      error("no such element")
    else {
      val it = x
      x = null
      it
    }
  }
}