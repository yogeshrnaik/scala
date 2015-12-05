import fp.chapter3.datastructures._
import fp.chapter3.datastructures.List._

/** **************************************************/
val ex1: List[Double] = Nil
val ex2: List[Int] = Cons(1, Nil)
val ex3: List[String] = Cons("a", Cons("b", Nil))

println(ex1)
println(ex2)
println(ex3)
/** **************************************************/
println(sum(List(1, 2)))
println(product(List(1, 2)))
println(product(List(2, 0, 10, 12)))
println(product(List(10, 12)))
/** **************************************************/
/**
 * EXERCISE 3.1
 * What will be the result of the following match expression?
 */
val x = List(1, 2, 3, 4, 5) match {
  case Cons(x, Cons(2, Cons(4, _))) => x
  case Nil => 42
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  case Cons(h, t) => h + sum(t)
  case _ => 101
}
/** **************************************************/
// Tail
println(tail(List(1, 4, 6)))
println(tail(List()))

/** **************************************************/
// set Head
println(setHead(1, List(2, 8, 9)))
//println(setHead(2, List()))
/** **************************************************/
// drop
println(drop(List(1, 2, 3), 2))
println(drop(List(1, 2, 3), -10))
println(drop(List(1, 2, 3), 10))

println(dropWithPatternMatch(List(38, 989, 10), 2))
println(dropWhile(List(1, 2, 3, 10, 5, 6))(x => x < 5))
/** **************************************************/
println(init(List(1,2)))
println(init(List(1)))
println(init(List(1,2,3,4)))
/** **************************************************/
println(initTailRecWithListReverse(List(1), List()))
println(initTailRecWithListReverse(List(1,2), List()))
println(initTailRecWithListReverse(List(1,2,3), List()))
/** **************************************************/
println(sumWithFoldRight(List(1,2,3,4,5)))
println(productWithFoldRight(List(1,2,3)))
/** **************************************************/
/**
 * EXERCISE 3.8
 * See what happens when you pass Nil and Cons themselves to foldRight, like this:
 * foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)).10 What do you think this
 * says about the relationship between foldRight and the data constructors of List?
 * ANSWER = Cons(1,Cons(2,Cons(3,Nil)))
 */
println(foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)))
/** **************************************************/
println(sumWithFoldLeft(List(1,2,3,4,5)))
println(productWithFoldLeft(List(1,2,3,4,5)))
println(lengthWithFoldLeft(List(1,2,3,4,5)))
/** **************************************************/
println(reverse(List(1,2,3)))
println(reverse(List(1)))
println(reverse(List()))
/** **************************************************/

