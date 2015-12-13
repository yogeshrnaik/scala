package fp.chapter4.errorhandling.validation

import fp.chapter4.errorhandling.either._
import fp.chapter3.datastructures.list.List

case class Person(name: Name, age: Age)

sealed class Name(val value: String)

sealed class Age(val value: Int)

object Person {
  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty.")
    else Right(new Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of range.")
    else Right(new Age(age))

  def mkPerson(name: String, age: Int): Either[List[String], Person] = {
    val name1: Either[String, Name] = mkName(name)
    val age1: Either[String, Age] = mkAge(age)

    name1.combine(age1)(Person(_, _))
//    name1.map2(age1)(Person(_, _))
  }
}
