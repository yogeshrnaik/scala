package week3

object rationals {
  new Rational(1, 2)                              //> res0: week3.Rational = 1/2
}

class Rational(x: Int, y:Int) {
	def numer = x
	def denom = y
	override def toString = x + "/" + y
}