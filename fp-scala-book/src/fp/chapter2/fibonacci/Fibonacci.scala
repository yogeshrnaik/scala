package fp.chapter2.fibonacci

import scala.annotation.tailrec

object Fibonacci {

  def fib(n: Int): Int = {

    @tailrec
    def nthFib(i: Int, n1: Int, n2: Int): Int = {
      if (i >= n) n1 // takes care of negative n
      else nthFib(i + 1, n2, n1 + n2)
    }

    nthFib(0, 0, 1)
  }
}
