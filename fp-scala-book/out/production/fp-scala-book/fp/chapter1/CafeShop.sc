import fp.chapter1.CreditCard
import fp.chapter1.Cafe

val cafe = new Cafe

val coffee = cafe.buyCoffee(new CreditCard())
println(coffee)

