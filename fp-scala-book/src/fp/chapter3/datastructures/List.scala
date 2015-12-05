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

  /**
   * EXERCISE 3.2
   * Implement the function tail for removing the first element of a List. Note that the
   * function takes constant time. What are different choices you could make in your
   * implementation if the List is Nil? We'll return to this question in the next chapter.
   */
  def tail[A](list: List[A]): List[A] =
    list match {
      case Nil => Nil
      case Cons(_, tail) => tail
    }

  /**
   * EXERCISE 3.3
   * Using the same idea, implement the function setHead for replacing the first element
   * of a List with a different value.
   */
  def setHead[A](newHead: A, list: List[A]): List[A] = list match {
    case Nil => sys.error("setHead on empty list")
    case Cons(_, tail) => Cons(newHead, tail)
  }

  /**
   * EXERCISE 3.4
   * Generalize tail to the function drop, which removes the first n elements from a list.
   * Note that this function takes time proportional only to the number of elements being dropped.
   * we don't need to make a copy of the entire List.
   */
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

  /**
   * EXERCISE 3.5
   * Implement dropWhile, which removes elements from the List prefix as long as they match a predicate.
   */
  @tailrec
  def dropWhile[A](list: List[A])(predicate: A => Boolean): List[A] = list match {
    case Cons(head, tail) if predicate(head) => dropWhile(tail)(predicate)
    case _ => list
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  /**
   * EXERCISE 3.6
   * Not everything works out so nicely. Implement a function, init, that returns a List
   * consisting of all but the last element of a List. So, given List(1,2,3,4), init will
   * return List(1,2,3). Why can't this function be implemented in constant time like tail?
   */
  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  /**
   * Returns a reversed list with last element removed
   */
  @tailrec
  def initTailRecWithListReverse[A](l: List[A], a: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(h, Nil) => a
      case Cons(h, t) => initTailRecWithListReverse(t, Cons(h, a))
    }
  }

  /**
   * EXERCISE 3.7
   * Can product, implemented using foldRight, immediately halt the recursion and
   * return 0.0 if it encounters a 0.0? Why or why not? Consider how any short-circuiting
   * might work if you call foldRight with a large list. This is a deeper question that we'll return to in chapter 5.
   * ANSWER - NO
   */
  def foldRight[A, B](as: List[A], initial: B)(reducer: (A, B) => B): B = {
    as match {
      case Nil => initial
      case Cons(h, t) => reducer(h, foldRight(t, initial)(reducer))
    }
  }

  def sumWithFoldRight(ints: List[Int]) = {
    foldRight(ints, 0)((x, y) => x + y)
  }

  def productWithFoldRight(numbers: List[Double]) = {
    foldRight(numbers, 1.0)(_ * _)
  }

  /**
   * EXERCISE 3.9
   * Compute the length of a list using foldRight.
   */
  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_, len) => len + 1)
  }

  /**
   * EXERCISE 3.10
   * Our implementation of foldRight is not tail-recursive and will result in a StackOverflowError
   * for large lists (we say it's not stack-safe). Convince yourself that this is the case,
   * and then write another general list-recursion function, foldLeft, that is tail-recursive,
   * using the techniques we discussed in the previous chapter. Here is its signature
   */
  def foldLeft[A, B](list: List[A], initial: B)(reducer: (B, A) => B): B = {
    list match {
      case Nil => initial
      case Cons(h, t) => foldLeft(t, reducer(initial, h))(reducer)
    }
  }

  /**
   * EXERCISE 3.11
   * Write sum, product, and a function to compute the length of a list using foldLeft.
   */
  def sumWithFoldLeft(ints: List[Int]) = foldLeft(ints, 0)(_ + _)
  def productWithFoldLeft(numbers: List[Double]) = foldLeft(numbers, 1.0)(_ * _)
  def lengthWithFoldLeft[A](list: List[A]): Int = foldLeft(list, 0)((len, _) => len + 1)

  /**
   * EXERCISE 3.12
   * Write a function that returns the reverse of a list (given List(1,2,3) it returns List(3,2,1)).
   * See if you can write it using a fold.
   */
  def reverse[A](list: List[A]): List[A] = {
    foldLeft(list, Nil: List[A])((acc, h) => Cons(h, acc))
  }

  /**
   * EXERCISE 3.13
   * Hard: Can you write foldLeft in terms of foldRight? How about the other way around?
   * Implementing foldRight via foldLeft is useful because it lets us implement foldRight tail-recursively,
   * which means it works even for large lists without overflowing the stack.
   */
  def foldLeftWithFoldRight[A, B](list: List[A], initial: B)(reducer: (B, A) => B): B = {
    sys.error("to do")
  }
  def foldRightWithFoldLeft[A, B](as: List[A], initial: B)(reducer: (A, B) => B): B = {
    sys.error("to do")
  }

  /**
   * EXERCISE 3.14
   * Implement append in terms of either foldLeft or foldRight.
   */
  def appendWithFoldRight[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight(a1, a2)(Cons(_, _))
  }

  /**
   * The function apply in the object List is a variadic function, meaning it accepts zero
   * or more arguments of type A:
   */
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}