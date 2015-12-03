import fp.chapter2.polymorphic.PolymorphicFunctions._

val ints = Array(7, 9, 13, 19, 23, 29)

println(findFirst(ints, (x: Int) => x == 9))

println(findFirst(ints, (x: Int) => x == 8))

println(findFirst(ints, (x: Int) => x > 15))