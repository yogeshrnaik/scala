import fp.chapter2.polymorphic.PolymorphicFunctions._

/*********************************************************/
val ints = Array(7, 9, 13, 19, 23, 29)

println(findFirst(ints, (x: Int) => x == 9))
println(findFirst(ints, (x: Int) => x == 8))
println(findFirst(ints, (x: Int) => x > 15))
/*********************************************************/
val lessThanEqualTo: (Int, Int) => Boolean = (x, y) => x <= y
println(isSorted(Array(10, 11, 12, 5), lessThanEqualTo))

println(isSorted(Array(10, 11, 12, 15), lessThanEqualTo))

println(isSorted(Array(1,2), lessThanEqualTo))
println(isSorted(Array(2,2), lessThanEqualTo))
println(isSorted(Array(2,1), lessThanEqualTo))

println(isSorted(Array(1), lessThanEqualTo))

println(isSorted(Array(), lessThanEqualTo))
println(isSorted(null, lessThanEqualTo))
/*********************************************************/
val stringCompare: (String, String) => Boolean = (x, y) => x <= y
println(isSorted(Array("Hi", "Hello"), stringCompare))
println(isSorted(Array("Hi", "Hi"), stringCompare))
println(isSorted(Array("Hello", "Hey"), stringCompare))
println(isSorted(Array("Hey", "Hello"), stringCompare))
/*********************************************************/
