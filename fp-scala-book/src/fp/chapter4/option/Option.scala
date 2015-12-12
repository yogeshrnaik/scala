package fp.chapter4.option

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

