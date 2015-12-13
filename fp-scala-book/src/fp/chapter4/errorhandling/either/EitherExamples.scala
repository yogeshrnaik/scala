package fp.chapter4.errorhandling.either

import fp.chapter4.errorhandling.either.Either.Try

object EitherExamples {
  def mean(xs: List[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)


  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    Try(x / y)


  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = age + numberOfSpeedingTickets + 100

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Either[Exception, Double] = {
    for {
      a <- Try(age.toInt)
      tickets <- Try(numberOfSpeedingTickets.toInt)
    } yield insuranceRateQuote(a, tickets)
  }
}

