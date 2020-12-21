package functionalprogramming.a_pure_function

object IO {

}

case class Player(name: String, score: Int)

class Contest {

  def contest(p1: Player, p2: Player) = {
    if (p1.score > p2.score) print(s"winner is ${p1.name}.")
    else if (p1.score < p2.score) print(s"winner is ${p2.name}.")
    else "It is a draw."
  }

  /**
   * 副作用分解
   * 非纯函数 A=>B 可以转化为 A=>C 和 C=>B两个函数，将副作用逻辑隔离
   */

  /**
   * 计算胜者
   * @param p1
   * @param p2
   * @return
   */
  def getWinner(p1: Player, p2: Player) = {
    if (p1.score > p2.score) Some(p1)
    else if (p1.score < p2.score) Some(p2)
    else None
  }

  /**
   * 结算消息
   * @param player
   * @return
   */
  def winnerMsg(player: Option[Player]) = player match {
    case Some(Player(name, _)) => s"winner is ${name}."
    case None => "It is a draw."
  }

  /**
   * 打印消息 - 副作用
   * @param p1
   * @param p2
   */
  def printWinner(p1: Player, p2: Player): Unit = {
    print(winnerMsg(getWinner(p1, p2)))
  }

  trait IO {
    def run: Unit
  }

  /**
   * 标示消息应该被打印到console，打印操作由run方法来控制
   * @param msg
   * @return
   */
  def printLine(msg: String): IO = new IO {
    override def run: Unit = println(msg)
  }

  def contest(p1: Player, p2: Player) = {
    print(winnerMsg(getWinner(p1, p2)))
  }

}





