package fp.chapter2.factorial

import scala.annotation.tailrec

object Factorial {
  def factorial(n: Int): Int = {
    @tailrec
    def go(n: Int, acc: Int): Int = if (n <= 0) acc else go(n - 1, n * acc)

    go(n, 1)
  }

  def fib(n: Int): Int = {

    @tailrec
    def nthFib(i: Int, n1: Int, n2: Int): Int = {
      if (i == n) n1
      else nthFib(i+1, n2, n1+n2)
    }

    nthFib(0, 0, 1)
  }
}
