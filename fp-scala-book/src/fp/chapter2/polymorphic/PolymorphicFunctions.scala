package fp.chapter2.polymorphic

object PolymorphicFunctions {
  def findFirst[A](array: Array[A], predicate : A => Boolean) : Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= array.length) -1
      else if (predicate(array(n))) n
      else loop(n + 1)

    loop(0)
  }
}
