package fp.chapter5.stream

import fp.chapter5.stream.Stream.{cons, empty}

import scala.annotation.tailrec

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  /**
   * EXERCISE 5.1
   * Write a function to convert a Stream to a List, which will force its evaluation and let you look at it in the REPL.
   * You can convert to the regular List type in the standard library.
   * You can place this and other functions that operate on a Stream inside the Stream trait.
   */
  def toListRecursive: List[A] = {
    // this version will stack overflow for large streams
    this match {
      case Empty => List()
      case Cons(h, t) => h() :: t().toListRecursive
    }
  }

  def toList: List[A] = {
    def toListTailrec(s: Stream[A], acc: List[A]): List[A] = {
      s match {
        case Empty => acc
        case Cons(head, tail) => toListTailrec(tail(), head() :: acc)
      }
    }
    toListTailrec(this, List()).reverse
  }

  /**
   * EXERCISE 5.2
   * Write the function take(n) for returning the first n elements of a Stream, and drop(n) for skipping the first n elements of a Stream.
   */
  def take(n: Int): Stream[A] = {
    this match {
      case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
      case Cons(h, _) if n == 1 => cons(h(), empty)
      case _ => empty
    }
  }

  @tailrec
  final def drop(n: Int): Stream[A] = {
    this match {
      case Cons(h, t) if n <= 0 => this
      case Cons(h, t) if n > 0 => t().drop(n - 1)
    }
  }

  /**
   * EXERCISE 5.3
   * Write the function takeWhile for returning all starting elements of a Stream that match the given predicate.
   */
  def takeWhile(predicate: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if predicate(h()) => cons(h(), t().takeWhile(predicate))
    case _ => Empty
  }

  @tailrec
  final def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) if p(h()) => true
    case Cons(h, t) => t().exists(p)
    case _ => false
  }
}

case object Empty extends Stream[Nothing]

/**
 * A nonempty stream consists of a head and a tail, which are both non-strict.
 * Due to technical limitations, these are thunks that must be explicitly forced, rather than by-name parameters.
 */
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  /** A smart constructor for creating a nonempty stream. */
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  /** A smart constructor for creating an empty stream of a particular type. */
  def empty[A]: Stream[A] = Empty

  /** A convenient variable-argument method for constructing a Stream from multiple elements. */
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}