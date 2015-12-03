/******************************************************/
val f = (x: Double) => math.Pi / 2 - x
val cos = f andThen math.sin

println(f(10))
println(cos(10))
/******************************************************/
val half = (x: Double) => x / 2
val oneThird = (x: Double) => x / 3
val minus2 = (x: Double) => x - 2

val oneFourth = half andThen half
val oneSixth = half compose oneThird

println(oneFourth(36))
println(oneSixth(36))
/******************************************************/
// f andThen g is the same as g compose f
val minus2ThenHalf = minus2 andThen half
val minus2ComposeHalf = minus2 compose half

println(minus2ThenHalf(10)) // (10 - 2) / 2 = 8 / 2 = 4
println(minus2ComposeHalf(10)) // (10/2) - 2 = 5 - 2 = 3
/******************************************************/