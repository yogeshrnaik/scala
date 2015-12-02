import fp.chapter1.cafe.Cafe
import fp.chapter1.cafe.{Charge, CreditCard}

val cafe = new Cafe

val (coffee, charge1) = cafe.buyCoffee(new CreditCard(1111))
println("Charge amount = " + charge1.amount)

val (coffees, charge5) = cafe.buyCoffees(new CreditCard(2222), 5)
println("Charge amount = " + charge5.amount)

val cc1 = CreditCard(1234)
val cc2 = CreditCard(9876)
val charges = cafe.coalesce(List(Charge(cc1, 10), Charge(cc1, 5), Charge(cc2, 20)))
println(charges)