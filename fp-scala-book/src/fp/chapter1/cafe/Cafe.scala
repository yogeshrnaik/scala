package fp.chapter1.cafe

class Cafe {
  def buyCoffee(cc: CreditCard): (Coffee, Charge) = {
    val cup = new Coffee(10)
    (cup, Charge(cc, cup.price))
  }

  def buyCoffees(cc: CreditCard, n: Int): (List[Coffee], Charge) = {
    val purchases: List[(Coffee, Charge)] = List.fill(n)(buyCoffee(cc))
    val (coffees, charges) = purchases.unzip
    (coffees, charges.reduce((c1,c2) => c1.combine(c2)))
  }

  def coalesce(charges: List[Charge]): List[Charge] =
    charges.groupBy(_.cc).values.map(_.reduce(_ combine _)).toList
}


case class Charge(cc: CreditCard, amount: Double) {
  def combine(other: Charge): Charge =
    if (cc == other.cc)
      Charge(cc, amount + other.amount)
    else
      throw new Exception("Can't combine charges to different cards")
}

class CreditCard

case class Coffee (price: Int) {
}

class CafeMain {
  def main(args: Array[String]) {
    val cafe = new Cafe
    val (coffee, charge) = cafe.buyCoffee(new CreditCard)
    println(coffee.price)
    println(charge.amount)
  }
}