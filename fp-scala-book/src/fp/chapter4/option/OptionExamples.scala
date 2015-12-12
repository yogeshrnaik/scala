package fp.chapter4.option

object OptionExamples {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def main(args: Array[String]) {
    val some = Some(10)
    println(some.map(a => a + 20))
  }
}
