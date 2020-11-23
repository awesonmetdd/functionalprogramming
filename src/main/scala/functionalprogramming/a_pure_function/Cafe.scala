package functionalprogramming.a_pure_function

class Cafe {

  ///有副作用的函数
  def buyCoffee(cc: CreditCard): Coffee = {
    val cup = Coffee()

    cc.charge(cup.prize)

    cup
  }

  //纯函数
//  def buyCoffee(cc: CreditCard): (Coffee, Charge) = {
//    val cup = Coffee()
//    (cup, Charge(cc, cup.prize))
//  }


  case class CreditCard() {
    def charge(prize: Int): Unit = {
      //计费
    }
  }

  case class Coffee(prize: Int = 1)

  case class Charge(cc: CreditCard, amount: Int) {
    def combine(other: Charge): Charge = {
      if (cc == other.cc)
        Charge(cc, amount + other.amount)
      else
        throw new Exception
    }
  }

}
