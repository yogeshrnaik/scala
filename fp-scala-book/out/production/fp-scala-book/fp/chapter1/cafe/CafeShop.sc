import fp.chapter1.cafe.Cafe
import fp.chapter1.cafe.{Charge, CreditCard}

val cafe = new Cafe

val (coffee, charge1) = cafe.buyCoffee(new CreditCard())
println("Charge amount = " + charge1.amount)

val (coffees, charge5) = cafe.buyCoffees(new CreditCard(), 5)
println("Charge amount = " + charge5.amount)

val cc1 = new CreditCard
val cc2 = new CreditCard
val charges = cafe.coalesce(List(Charge(cc1, 10), Charge(cc1, 5), Charge(cc2, 20)))
println(charges)