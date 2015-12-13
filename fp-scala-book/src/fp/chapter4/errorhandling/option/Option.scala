package fp.chapter4.errorhandling.option

import fp.chapter3.datastructures.list._
import fp.chapter3.datastructures.list.List._

import scala.annotation.tailrec

/**
 * EXERCISE 4.1
 * Implement all of the preceding functions on Option.
 * As you implement each function, try to think about what it means and in what situations you'd use it.
 * We'll explore when to use each of these functions next. Here are a few hints for solving this exercise:
 * - It's fine to use pattern matching, though you should be able to implement all the functions besides map and getOrElse without resorting to pattern matching.
 * - For map and flatMap, the type signature should be enough to determine the implementation.
 * - getOrElse returns the result inside the Some case of the Option, or if the Option is None, returns the given default value.
 * - orElse returns the first Option if it's defined; otherwise, it returns the second Option.
 */
sealed trait Option[+A] {
  /** Apply f if the Option is not None. */
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  /** Apply f, which may fail, to the Option if not None. */
  def flatMap[B](f: A => Option[B]): Option[B] = {
    this match {
      case None => None
      case Some(a) => f(a)
    }
    // map(f).getOrElse(None) // more concise
  }

  /** Convert Some to None if the value doesn't satisfy f. */
  def filter(f: A => Boolean): Option[A] = {
    /*this match {
      case Some(a) if f(a) => this
      case _ => None
    }*/
    flatMap((a: A) => if (f(a)) this else None) // more concise
  }

  /** The B >: A says that the B type parameter must be a supertype of A. */
  def getOrElse[B >: A](default: => B): B = {
    this match {
      case None => default
      case Some(a) => a
    }
  }

  /** Don't evaluate ob unless needed. */
  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    this match {
      case None => ob
      case Some(a) => this
    }
    // map((a: A) => Some(a)).getOrElse(ob) // more concise
    // map(Some(_)).getOrElse(ob) // more concise
    // map(Some(_)) getOrElse ob // most concise
  }
}

case object None extends Option[Nothing]

case class Some[+A](get: A) extends Option[A]

object Option {
  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  val absO: Option[Double] => Option[Double] = lift(math.abs)

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch {
      case e: Exception => None
    }

  /**
   * EXERCISE 4.3
   * Write a generic function map2 that combines two Option values using a binary function.
   * If either Option value is None, then the return value is too. Here is its signature:
   */
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    (a, b) match {
      case (None, _) => None
      case (_, None) => None
      case (Some(a), Some(b)) => Some(f(a, b))
    }
    // a.flatMap((aa: A) => b.map((bb: B) => f(aa, bb))) // more concise
    // a flatMap (aa => b map (bb => f(aa, bb))) // more concise
  }

  /**
   * EXERCISE 4.4
   * Write a function sequence that combines a list of Options into one Option containing
   * a list of all the Some values in the original list. If the original list contains None even once,
   * the result of the function should be None; otherwise the result should be Some with a list of all the values.
   */
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    /*a match {
      case Nil => Some(Nil)
      case Cons(h, t) => h.flatMap((hh: A) => sequence(t).map((tt: List[A]) => Cons(hh, tt)))
      // case Cons(h, t) => h.flatMap(hh => sequence(t).map(Cons(hh, _))) // more concise
    }*/

    // another possible implementation that is hopefully more easy to understand
    a match {
      case Nil => Some(Nil)
      case Cons(h, t) => h match {
        case None => None
        case Some(a) => sequence(t).map(tailAfterApplyingSequence => Cons(a, tailAfterApplyingSequence))
      }
    }
  }

  def parseInts(a: List[String]): Option[List[Int]] = sequence(map(a)(i => Try(i.toInt)))

  /**
   * EXERCISE 4.5
   * Implement this function. It's straightforward to do using map and sequence,
   * but try for a more efficient implementation that only looks at the list once.
   * In fact, implement sequence in terms of traverse.
   */
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a match {
      case Nil => Some(Nil)
      // case Cons(h, t) => map2(f(h), traverse(t)(f))((hh: B, tt: List[B]) => Cons(hh, tt))
      case Cons(h, t) => map2(f(h), traverse(t)(f))(Cons(_, _)) // more concise
    }
  }

  def traverseWithFoldRight[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    // foldRight(a, (Some(Nil)): Option[List[B]])((h: A, acc: Option[List[B]]) => map2(f(h), acc)((hh: B, tt: List[B]) => Cons(hh, tt)))
    foldRight(a, Some(Nil): Option[List[B]])((h, acc) => map2(f(h), acc)(Cons(_, _))) // more concise
  }

  def sequenceWithTraverse[A](a: List[Option[A]]): Option[List[A]] = {
    traverse(a)((aOption: Option[A]) => aOption)
  }
}