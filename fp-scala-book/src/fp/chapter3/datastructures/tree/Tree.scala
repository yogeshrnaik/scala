package fp.chapter3.datastructures.tree

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  /**
   * EXERCISE 3.25
   * Write a function size that counts the number of nodes (leaves and branches) in a tree.
   */
  def size[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }
  }

  /**
   * EXERCISE 3.26
   * Write a function maximum that returns the maximum element in a Tree[Int].
   * (Note: In Scala, you can use x.max(y) or x max y to compute the maximum of two integers x and y.)
   */
  def max(t: Tree[Int]): Int = {
    t match {
      case Leaf(v) => v
      case Branch(left, right) => max(left).max(max(right))
    }
  }

  /**
   * EXERCISE 3.27
   * Write a function depth that returns the maximum path length from the root of a tree to any leaf.
   */
  def depth(t: Tree[Int]): Int = {
    t match {
      case Leaf(_) => 0
      case Branch(left, right) => 1 + depth(left).max(depth(right))
    }
  }

  /**
   * EXERCISE 3.28
   * Write a function map, analogous to the method of the same name on List, that modifies each element in a tree with a given function.
   */
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    t match {
      case Leaf(v) => Leaf(f(v))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }
  }

  /**
   * EXERCISE 3.29
   * Generalize size, maximum, depth, and map, writing a new function fold that abstracts over their similarities.
   * Reimplement them in terms of this more general function. Can you draw an analogy between this fold function and the left and right folds for List?
   */
  def fold[A, B](t: Tree[A])(transform: A => B)(combine: (B, B) => B): B = t match {
    case Leaf(a) => transform(a)
    case Branch(left, right) => combine(fold(left)(transform)(combine), fold(right)(transform)(combine))
  }

  def sizeWithFold[A](t: Tree[A]): Int = {
    fold(t)(a => 1)((b1: Int, b2: Int) => 1 + b1 + b2)
  }

  def maxWithFold(t: Tree[Int]): Int = {
    fold(t)(a => a)((b1, b2) => b1.max(b2))
  }

  def depthWithFold[A](t: Tree[A]): Int = {
    fold(t)(a => 0)((d1, d2) => 1 + d1.max(d2))
  }

  def mapWithFold[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    fold(t)(a => Leaf(f(a)): Tree[B])((b1: Tree[B], b2: Tree[B]) => Branch(b1, b2))
  }
}