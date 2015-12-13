package fp.chapter4.errorhandling.employee

import fp.chapter4.errorhandling.option

case class Employee(name: String, department: String, manager: option.Option[String])

object Employee {
  def apply(name: String, department: String) = new Employee(name, department, option.None)

  def apply(name: String, department: String, manager: String) = new Employee(name, department, option.Some(manager))

  def lookupByName(name: String): option.Option[Employee] = {
    if (name == "Joe") option.Some(Employee("Joe", "Accounting", "Hans"))
    else if (name == "John") option.Some(Employee("John", "Manufacturing"))
    else option.None
  }
}