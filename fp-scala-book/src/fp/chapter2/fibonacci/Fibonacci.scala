package fp.chapter2.fibonacci

import scala.annotation.tailrec

object Fibonacci {


  /**
   * EXERCISE 2.1
   * Write a recursive function to get the nth Fibonacci number (http://mng.bz/C29s).
   * The first two Fibonacci numbers are 0 and 1. The nth number is always the sum of the
   * previous two-the sequence begins 0, 1, 1, 2, 3, 5. Your definition should use a local tail-recursive function.
   */
  def fib(n: Int): Int = {
    @tailrec
    def nthFib(i: Int, n1: Int, n2: Int): Int = {
      if (i >= n) n1 // takes care of negative n
      else nthFib(i + 1, n2, n1 + n2)
    }

    nthFib(0, 0, 1)
  }
}
