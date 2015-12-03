package fp.chapter3.datastructures

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = {
    @tailrec
    def tailrecSum(sum: Int, list: List[Int]): Int = {
      list match {
        case Nil => sum
        case Cons(head, tail) => tailrecSum(sum + head, tail)
      }
    }
    tailrecSum(0, ints)
  }

  def product(ds: List[Double]): Double = {
    @tailrec
    def tailrecProduct(product: Double, list: List[Double]): Double = {
      list match {
        case Nil => 1.0
        case Cons(0.0, _) => 0.0
        case Cons(head, tail) => tailrecProduct(product * head, tail)
      }
    }
    tailrecProduct(1, ds)
  }

  def tail[A](list: List[A]): List[A] =
    list match {
      case Nil => Nil
      case Cons(_, tail) => tail
    }

  def setHead[A](newHead: A, list: List[A]): List[A] = list match {
    case Nil => sys.error("setHead on empty list")
    case Cons(_, tail) => Cons(newHead, tail)
  }

  def drop[A](list: List[A], n: Int): List[A] = {
    @tailrec
    def dropTailRec(l: List[A], i: Int): List[A] = if (i == n) l else dropTailRec(tail(l), i + 1)

    if (n < 0) list else dropTailRec(list, 0)
  }

  @tailrec
  def dropWithPatternMatch[A](list: List[A], n: Int): List[A] = {
    if (n <= 0) list
    else
      list match {
        case Nil => Nil
        case Cons(_, tail) => dropWithPatternMatch(tail, n - 1)
      }
  }

  @tailrec
  def dropWhile[A](list: List[A], predicate: A => Boolean): List[A] = list match {
    case Cons(head, tail) if predicate(head) => dropWhile(tail, predicate)
    case _ => list
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  // TODO: does not work
  def innerAppend[A](a1: List[A], a2: List[A], acc: List[A]) : List[A] = a1 match {
    case Nil => a2
    case Cons(h, Nil) => Cons (h, acc)
    case Cons(h, t) => innerAppend(t, a2, Cons(h, a2))
  }

  /**
   * The function apply in the object List is a variadic function, meaning it accepts zero
   * or more arguments of type A:
   */
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}