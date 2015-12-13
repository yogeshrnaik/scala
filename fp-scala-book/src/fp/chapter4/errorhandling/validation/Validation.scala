package fp.chapter4.errorhandling.validation

/**
 * EXERCISE 4.8
 * In this implementation, map2 is only able to report one error, even if both the name  and the age are invalid.
 * What would you need to change in order to report both errors?
 * Would you change map2 or the signature of mkPerson?
 * Or could you create a new data type that captures this requirement better than Either does, with some additional structure?
 * How would orElse, traverse, and sequence behave differently for that data type?
 */

trait Validation[+A, +B]

case class Errors[+A](get: Seq[A]) extends Validation[A, Nothing]

case class Success[+B](get: B) extends Validation[Nothing, B]

object Validation {
  sys.error("todo")
}
