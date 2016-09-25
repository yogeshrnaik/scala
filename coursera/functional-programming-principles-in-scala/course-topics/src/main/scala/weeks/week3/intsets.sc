package weeks.week3

object intsets {
  val t1 = new NonEmpty(3, Empty, Empty)          //> t1  : weeks.week3.NonEmpty = {.3.}
  val t2 = t1 incl 4                              //> t2  : weeks.week3.IntSet = {.3{.4.}}
  t2 union new NonEmpty(1, Empty, Empty)          //> res0: weeks.week3.IntSet = {.1{{.3.}4.}}
}

abstract class IntSet {
  def contains(x: Int): Boolean
  def incl(x: Int): IntSet
  def union(other: IntSet) : IntSet
}

object Empty extends IntSet {
  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
  def union(other: IntSet) = other
  override def toString = "."
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def contains(x: Int): Boolean =
  	if (x < elem) left.contains(x)
  	else if (x > elem) right.contains(x)
  	else true
  
	def incl(x: Int): IntSet =
		if (x < elem) new NonEmpty(elem, left incl x, right)
		else if (x > elem) new NonEmpty(elem, left, right incl x)
		else this
		
	def union(other: IntSet) = ((left union right) union other) incl elem
	
	override def toString = "{" + left + elem + right + "}"
}