package weeks.week3

object rationals {
	def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
                                                  //> gcd: (a: Int, b: Int)Int
	
  new Rational(1, 2) + new Rational(2, 3)         //> res0: weeks.week3.Rational = 7/6
  val r = new Rational(1, 3)                      //> r  : weeks.week3.Rational = 1/3
  gcd(-1, 2)                                      //> res1: Int = -1
	-r                                        //> res2: weeks.week3.Rational = 1/-3

	val x = new Rational(1, 3)                //> x  : weeks.week3.Rational = 1/3
	val y = new Rational(5, 7)                //> y  : weeks.week3.Rational = 5/7
	val z = new Rational(3, 2)                //> z  : weeks.week3.Rational = 3/2
 
	x - y - z                                 //> res3: weeks.week3.Rational = -79/42
	y + y                                     //> res4: weeks.week3.Rational = 10/7
	x < y                                     //> res5: Boolean = true
	x.max(y)                                  //> res6: weeks.week3.Rational = 5/7
	
	new Rational(2)                           //> res7: weeks.week3.Rational = 2/1
}

class Rational(x: Int, y:Int) {
	require(y != 0, "denominator must be non-zero")
	
	def this(x: Int) = this(x, 1)

	private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
	private val g = gcd(x,y)
	val numer = x / g
	val denom = y / g
	
	def < (that: Rational) = numer * that.denom < that.numer * denom
	
	def max(that: Rational) = if (this < that) that else this
	
	def + (that: Rational) = new Rational(
			numer * that.denom + that.numer * denom,
			denom * that.denom)
	
	def unary_- : Rational = new Rational(-numer, denom)
	
	def - (that: Rational): Rational = this + -that
	
	override def toString = numer + "/" + denom
}