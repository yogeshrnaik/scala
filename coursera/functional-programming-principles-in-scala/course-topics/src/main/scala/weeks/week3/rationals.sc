package weeks.week3

object rationals {
  new Rational(1, 2).add(new Rational(2, 3))
  new Rational(1, 2).neg
}

class Rational(x: Int, y:Int) {
	def numer = x
	def denom = y
	
	def add(that: Rational) = new Rational(
			numer * that.denom + that.numer * denom,
			denom * that.denom)
	
	def neg: Rational = new Rational(-numer, denom)
	
	override def toString = numer + "/" + denom
}