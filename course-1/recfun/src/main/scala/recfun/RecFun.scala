package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    (c, r) match {
      case (0, _) => 1
      case (_, 0) => 1
      case (_, 1) => 1
      case (c, r) if c == r => 1
      case (c, r) => pascal(c - 1, r - 1 ) + pascal(c, r - 1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def innerBalance(c: List[Char], list: List[Char]): Boolean = {
      if (c.isEmpty && list.isEmpty) true
      else if (c.isEmpty && list.nonEmpty) false
      else {
        c.head match {
          case '(' => innerBalance(c.tail, '(' :: list)
          case ')' if list.isEmpty => false
          case ')' => innerBalance(c.tail, list.tail)
          case _ => innerBalance(c.tail, list)
        }
      }
    }
    innerBalance(chars, Nil)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0 || coins.isEmpty) 0
    else {
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
  }
}
