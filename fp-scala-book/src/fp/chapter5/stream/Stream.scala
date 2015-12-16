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


  /** The arrow => in front of the argument type B means that
    * the function f takes its second argument by name and may choose not to evaluate it. */
  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def existsWithFoldRight(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  /**
   * EXERCISE 5.4
   * Implement forAll, which checks that all elements in the Stream match a given predicate.
   * Your implementation should terminate the traversal as soon as it encounters a nonmatching value.
   */
  @tailrec
  final def forAll(p: A => Boolean): Boolean = {
    this match {
      case Cons(h, t) if p(h()) => t().forAll(p)
      case Cons(h, t) => false
      case _ => true
    }
  }

  /**
   * EXERCISE 5.5
   * Use foldRight to implement takeWhile.
   */
  def takeWhileWithFoldRight(predicate: A => Boolean): Stream[A] = {
    foldRight(empty: Stream[A])((h, t) => if (predicate(h)) cons(h, t) else empty)
  }

  /**
   * EXERCISE 5.6
   * Hard: Implement headOption using foldRight.
   */
  def headOptionWithFoldRight: Option[A] =
    foldRight(None: Option[A])((h, _) => Some(h))

  /**
   * EXERCISE 5.7
   * Implement map, filter, append, and flatMap using foldRight. The append method should be non-strict in its argument.
   */
  def map[B](f: A => B): Stream[B] = {
    foldRight(empty[B])((h, t) => cons(f(h), t))
  }

  def filter(f: A => Boolean): Stream[A] = {
    foldRight(empty[A])((h, t) => if (f(h)) cons(h, t) else t)
  }

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h) append t)

  def find(p: A => Boolean): Option[A] =
    filter(p).headOption
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


  /**
   * EXERCISE 5.8
   * Generalize ones slightly to the function constant, which returns an infinite Stream of a given value.
   */
  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  /**
   * EXERCISE 5.9
   * Write a function that generates an infinite stream of integers, starting from n, then n + 1, n + 2, and so on.7
   */
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  /**
   * EXERCISE 5.10
   * Write a function fibs that generates the infinite stream of Fibonacci numbers: 0, 1, 1, 2, 3, 5, 8, and so on.
   */
  def fibs(): Stream[Int] = {
    def innerFibs(n1: Int, n2: Int): Stream[Int] = {
      Stream.cons(n1, innerFibs(n2, n1 + n2))
    }

    innerFibs(0, 1)
  }

  /**
   * EXERCISE 5.11
   * Write a more general stream-building function called unfold. It takes an initial state,
   * and a function for producing both the next state and the next value in the generated stream.
   */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f)) // f produces Option[(A, S)] i.e. a pair containing A and S
      case None => empty
    }
  }

  /**
   * EXERCISE 5.12
   * Write fibs, from, constant, and ones in terms of unfold.
   */
  def fibsWithUnfold(): Stream[Int] = {
    unfold((0, 1))((pair) => pair match {
      case (f1, f2) => Some(f1, (f2, (f1 + f2)))
    })
  }

  def fromWithUnfold(n: Int) = {
    unfold(n)(i => Some(i, i + 1))
  }

  def constantWithUnfold[A](a: A): Stream[A] = unfold(a)(i => Some(i, i))

  def onesWithUnfold: Stream[Int] = unfold(1)(i => Some(i, i))


  /** A convenient variable-argument method for constructing a Stream from multiple elements. */
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}