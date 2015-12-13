package fp.chapter4.errorhandling.option

import fp.chapter3.datastructures.list.List

object OptionMain {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  /**
   * EXERCISE 4.2
   * Implement the variance function in terms of flatMap. If the mean of a sequence is m,
   * the variance is the mean of math.pow(x - m, 2) for each element x in the sequence.
   * See the definition of variance on Wikipedia (http://mng.bz/0Qsr).
   */
  def variance(xs: Seq[Double]): Option[Double] = {
    // mean(xs).flatMap((m: Double) => mean(xs.map((x: Double) => math.pow(x - m, 2))))
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  def main(args: Array[String]) {
    val some = Some(10)
    println(some.map(a => a + 20))

    val listOfSomeInts: List[Option[Int]] = List(Some(10), Some(20), Some(30))
    println(listOfSomeInts)
    println(Option.sequence(listOfSomeInts))

    println(Option.sequence(List()))
    println(Option.sequence(List(None, Some(1))))
  }
}