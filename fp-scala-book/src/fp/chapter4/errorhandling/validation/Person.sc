import fp.chapter4.errorhandling.validation.Person._

var person = mkPerson("", -1)
println(person)

person = mkPerson("", 25)
println(person)

person = mkPerson("Bill", -1)
println(person)

person = mkPerson("Bill", 25)
println(person)