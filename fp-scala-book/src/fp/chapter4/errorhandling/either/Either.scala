package fp.chapter4.errorhandling.either

import fp.chapter3.datastructures.list._

import fp.chapter3.datastructures.list.List._

/**
 * Either has only two cases, just like Option. 
 * The essential difference is that both cases carry a value. 
 * The Either data type represents, in a very general way, values that can be one of two things. 
 * We can say that it's a disjoint union of two types. 
 * When we use it to indicate success or failure, 
 * by convention the Right constructor is reserved for the success case (a pun on "right" meaning correct),
 * and Left is used for failure. We've given the left type parameter the suggestive name E (for error).
 */

/**
 * EXERCISE 4.6
 * Implement versions of map, flatMap, orElse, and map2 on Either that operate on the Right value.
 */
sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = {
    this match {
      case Left(e) => Left(e)
      case Right(a) => Right(f(a))
    }
  }

  /**
   * When mapping over the right side, we must promote the left type parameter to some supertype,
   * to satisfy the +E variance annotation.
   */
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
    this match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = {
    this match {
      case Left(_) => b
      case Right(_) => this
    }
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    (this, b) match {
      case (Left(e), _) => Left(e)
      case (_, Left(e)) => Left(e)
      case (Right(a1), Right(b1)) => Right(f(a1, b1))
    }
    // for { a: Either <- this; b1: Either <- b } yield f(a, b1)
  }

  /**
   * EXERCISE 4.7
   * Implement sequence and traverse for Either.
   * These should return the first error that's encountered, if there is one.
   */
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    this.traverse(es)((x: Either[E, A]) => x)
  }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    as match {
      case Nil => Right(Nil)
      // case Cons(h, t) => f(h).map2(traverse(t)(f))((hh: B, tt: List[B]) => Cons(hh, tt))
      case Cons(h, t) => f(h).map2(traverse(t)(f))(Cons(_, _)) // more concise
    }
  }

  def traverseWithFoldRight[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    // foldRight(as, Right(Nil): Either[E, List[B]])((h, acc) => f(h).map2(acc)((hh: B, tt: List[B]) => Cons(hh, tt)))
    foldRight(as, Right(Nil): Either[E, List[B]])((h, acc) => f(h).map2(acc)(Cons(_, _))) // more concise
  }
}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }
}