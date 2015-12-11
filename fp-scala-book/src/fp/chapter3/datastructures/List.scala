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
  def foldRight[A, B](list: List[A], initial: B)(reducer: (A, B) => B): B = {
    list match {
      case Nil => initial
      // reducer is a function that takes two arguments of type A and B and returns a result of type B
      case Cons(h, t) => reducer(h, foldRight(t, initial)(reducer))
    }
  }

  def sumWithFoldRight(ints: List[Int]) = {
    foldRight(ints, 0)((x: Int, y: Int) => x + y)
    // foldRight(ints, 0)((x, y) => x + y) // more concise
  }

  def productWithFoldRight(numbers: List[Double]) = {
    foldRight(numbers, 1.0)(_ * _) // most concise
  }

  /**
   * EXERCISE 3.9
   * Compute the length of a list using foldRight.
   */
  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_, len: Int) => len + 1)
  }

  /**
   * EXERCISE 3.10
   * Our implementation of foldRight is not tail-recursive and will result in a StackOverflowError
   * for large lists (we say it's not stack-safe). Convince yourself that this is the case,
   * and then write another general list-recursion function, foldLeft, that is tail-recursive,
   * using the techniques we discussed in the previous chapter. Here is its signature
   */
  @tailrec
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
    foldLeft(list, Nil: List[A])((acc: List[A], h: A) => Cons(h, acc))
    // foldLeft(list, Nil: List[A])((acc, h) => Cons(h, acc))
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
  def appendWithFold[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight(a1, a2)((h: A, acc: List[A]) => Cons(h, acc))
    // foldRight(a1, a2)((h, acc) => Cons(h, acc))
  }

  /**
   * EXERCISE 3.15
   * Hard: Write a function that concatenates a list of lists into a single list.
   * Its runtime should be linear in the total length of all lists.
   * Try to use functions we have already defined.
   */
  def concat[A](l: List[List[A]]): List[A] = {
    // foldRight(l, Nil:List[A])((x: List[A], y: List[A]) => append(x, y))
    // foldRight(l, Nil: List[A])((a, acc) => append(a, acc)) // more concise
    // foldRight(l, Nil:List[A])(append) // most concise
    foldLeft(l, Nil: List[A])(append)
  }

  /**
   * EXERCISE 3.16
   * Write a function that transforms a list of integers by adding 1 to each element.
   * (Reminder: this should be a pure function that returns a new List!)
   */
  def addOne(l: List[Int]): List[Int] = {
    /*l match {
      case Nil => Nil
      case Cons(h, t) => Cons(h+1, addOne(t))
    }*/
    foldRight(l, Nil: List[Int])((h: Int, t: List[Int]) => Cons(h + 1, t))
    // if we use foldLeft then that reverses the list
    // foldLeft(l, Nil:List[Int])((t: List[Int], h: Int) => Cons(h+1, t))
  }

  /**
   * EXERCISE 3.17
   * Write a function that turns each value in a List[Double] into a String.
   * You can use the expression d.toString to convert some d: Double to a String.
   */
  def doubleListToStringList(ds: List[Double]): List[String] = {
    foldRight(ds, Nil: List[String])((h: Double, acc: List[String]) => Cons(h.toString, acc))
  }

  /**
   * EXERCISE 3.18
   * Write a function map that generalizes modifying each element in a list while maintaining the structure of the list.
   * Here is its signature: def map[A,B](as: List[A])(f: A => B): List[B]
   */
  def map[A, B](as: List[A])(f: A => B): List[B] = {
    /*as match {
      case Nil => Nil:List[B]
      case Cons(h, t) => Cons(f(h), map(t)(f))
    }*/
    foldRight(as, Nil: List[B])((h: A, t: List[B]) => Cons(f(h), t))
  }

  /**
   * EXERCISE 3.19
   * Write a function filter that removes elements from a list unless they satisfy a given predicate.
   * Use it to remove all odd numbers from a List[Int].
   */
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    /*as match {
      case Nil => Nil
      case Cons(h, t) if f(h) => Cons(h, filter(t)(f))
    }*/
    foldRight(as, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)
  }

  /**
   * EXERCISE 3.20
   * Write a function flatMap that works like map except that the function given will return a list instead of a single result,
   * and that list should be inserted into the final resulting list. Here is its signature:
   * def flatMap[A,B](as: List[A])(f: A => List[B]): List[B]
   * For instance, flatMap(List(1,2,3))(i => List(i,i)) should result in List(1,1,2,2,3,3).
   */
  def flatMapWithFoldRight[A, B](list: List[A])(f: A => List[B]): List[B] = {
    foldRight(list, Nil: List[B])((h: A, t: List[B]) => append(f(h), t))
  }

  def flatMapWithConcat[A, B](list: List[A])(f: A => List[B]): List[B] = {
    concat(map(list)(f))
  }

  /**
   * EXERCISE 3.21
   * Use flatMap to implement filter.
   */
  def filterWithFlatMap[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMapWithConcat(as)(a => if (f(a)) List(a) else Nil)
  }

  /**
   * EXERCISE 3.22
   * Write a function that accepts two lists and constructs a new list by adding corresponding elements.
   * For example, List(1,2,3) and List(4,5,6) become List(5,7,9).
   */
  def addPairs(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairs(t1, t2))
  }

  /**
   * EXERCISE 3.23
   * Generalize the function you just wrote so that it's not specific to integers or addition.
   * Name your generalized function zipWith.
   */
  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] =
    (l1, l2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }

  /**
   * Returns a list consisting of the first n elements of this
   */
  def take[A](list: List[A], n: Int): List[A] = {
    @tailrec
    def innerTake(l: List[A], acc: List[A], i: Int): List[A] =
      if (i == n) acc
      else
        l match {
          case Nil => acc
          case Cons(h, t) => innerTake(t, Cons(h, acc), i + 1)
        }
    if (n <= 0) Nil else reverse(innerTake(list, Nil, 0))
  }

  /**
   * Returns a list consisting of the longest valid prefix of this whose elements all pass the predicate f
   */
  def takeWhile[A](list: List[A])(predicate: A => Boolean): List[A] = {
    @tailrec
    def innerTakeWhile(l: List[A], acc: List[A]): List[A] =
      l match {
        case Nil => acc
        case Cons(h, t) if predicate(h) => innerTakeWhile(t, Cons(h, acc))
        case Cons(h, t) => acc
      }

    reverse(innerTakeWhile(list, Nil))
  }

  /**
   * Returns true if and only if all elements of this pass the predicate f
   */
  @tailrec
  def forall[A](list: List[A])(predicate: A => Boolean): Boolean = {
    list match {
      case Nil => false
      case Cons(head, Nil) => predicate(head)
      case Cons(head, tail) => if (!predicate(head)) false else forall(tail)(predicate)
    }
  }

  /**
   * Returns true if any element of this passes the predicate f
   */
  def exists[A](list: List[A])(predicate: A => Boolean): Boolean = {
    list match {
      case Nil => false
      case Cons(head, Nil) => predicate(head)
      case Cons(head, tail) => if (predicate(head)) true else exists(tail)(predicate)
    }
  }

  /**
   * scanLeft and scanRight - Like foldLeft and foldRight, but they return the List of partial results rather than just the final accumulated value
   */
  def scanLeft[A, B, C](list: List[A], initial: B)(f: (A, B) => C): List[C] = {
    sys.error("to do")
  }

  def scanRight[A, B, C](list: List[A], initial: B)(f: (B, A) => C): List[C] = {
    sys.error("to do")
  }

  /**
   * EXERCISE 3.24
   * Hard: As an example, implement hasSubsequence for checking whether a List contains another List as a subsequence.
   * For instance, List(1,2,3,4) would have List(1,2), List(2,3), and List(4) as subsequences, among others.
   * You may have some difficulty finding a concise purely functional implementation that is also efficient.
   * That's okay. Implement the function however comes most naturally.
   * We'll return to this implementation in chapter 5 and hopefully improve on it.
   * Note: Any two values x and y can be compared for equality in Scala using the expression x == y.
   */
  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    sup match {
      case Nil => sub == Nil
      case _ if startsWith(sup, sub) => true
      case Cons(_, tail) => hasSubsequence(tail, sub)
    }
  }

  @tailrec
  def startsWith[A](list: List[A], prefix: List[A]): Boolean = {
    (list, prefix) match {
      case (_, Nil) => true
      case (Cons(h1, t1), Cons(h2, t2)) if (h1 == h2) => startsWith(t1, t2)
      case _ => false
    }
  }

  /**
   * The function apply in the object List is a variadic function, meaning it accepts zero
   * or more arguments of type A:
   */
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}