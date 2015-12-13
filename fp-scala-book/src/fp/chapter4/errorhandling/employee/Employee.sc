import fp.chapter4.errorhandling.employee.Employee
import Employee._
import fp.chapter4.errorhandling.employee.Employee
import fp.chapter4.errorhandling.option.Option

println("**************************************************")
val joeDepartment: Option[String] = lookupByName("Joe").map((e: Employee) => e.department)
println(joeDepartment)
println("**************************************************")
println(lookupByName("Alex").map(_.department)) // more concise
println("**************************************************")
println(lookupByName("Alex").map(_.department).getOrElse("Default Dept."))
println("**************************************************")
println(lookupByName("Joe").flatMap(_.manager))
println(lookupByName("Alex").flatMap(_.manager))
println("**************************************************")
println(lookupByName("John").flatMap(_.manager))
println("**************************************************")
println(lookupByName("Joe").map(_.department).filter(_ != "Accounting").getOrElse("Default Dept"))
println(lookupByName("Joe").map(_.department).filter(_ == "Accounting").getOrElse("Default Dept"))
println("**************************************************")