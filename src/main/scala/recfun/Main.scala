package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    //println(countChange(4,List(1,2)))
    //println(balance("(".toList))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if (c == r) 1 else if (c == 0) 1 else
      pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def aux(x: List[Char], count: Int): Boolean =
      if (x.isEmpty && count == 0) true else if (x.isEmpty && count != 0) false else if (count < 0) false else if (x.head == '(') aux(x.tail, count + 1) else if (x.head == ')') aux(x.tail, count - 1) else
        aux(x.tail, count)

    aux(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    if (money == 0) 1 else if (money < 0) 0 else if (coins.isEmpty) 0 else
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
}
