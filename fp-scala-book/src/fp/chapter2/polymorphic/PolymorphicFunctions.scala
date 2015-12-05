package fp.chapter2.polymorphic

object PolymorphicFunctions {
  def findFirst[A](array: Array[A], predicate: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= array.length) -1
      else if (predicate(array(n))) n
      else loop(n + 1)

    loop(0)
  }

  /**
   * EXERCISE 2.2
   * Implement isSorted, which checks whether an Array[A] is sorted according to a given comparison function:.
   */
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {

    def sorted(index: Int): Boolean = {
      if (as == null || as.length == 0 || index == as.length - 1) true // end of array
      else if (!ordered(as(index), as(index + 1))) false
      else sorted(index + 1)
    }

    sorted(0)
  }
}
