package fp.chapter5.laziness

object Laziness {
  def if1[A](cond: Boolean, onTrue: () => A, onFalse: () => A): A =
    if (cond) onTrue() else onFalse()

  def if2[A](cond: Boolean, onTrue: => A, onFalse: => A): A =
    if (cond) onTrue else onFalse

  def maybeTwiceStrict(b: Boolean, i: => Int) = if (b) i + i else 0

  def maybeTwiceLazy(b: Boolean, i: => Int) = {
    lazy val j = i
    if (b) j + j else 0
  }
}
