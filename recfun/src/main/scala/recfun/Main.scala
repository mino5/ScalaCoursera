package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println(balance("())(".toList)).toString()
    println(countChange(300, List(5, 10, 20, 50, 100, 200, 500)))
    //println(countChange(4, List(1, 2, 4)))

    println(sum(x => x * x)(5, 5));


  }

  def sqrt(x: Double) = averageDamp(y => x / y)(1)
  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else
      pascal(c - 1, r - 1) + pascal(c, r - 1)
  }


  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    def loop(pos: Int, balance: Int): Boolean = {

      if (pos == chars.length)
        return balance == 0
      else if (chars(pos) == '(')
        return loop(pos + 1, balance + 1)
      else if (chars(pos) == ')') {
        if (balance > 0)
          return loop(pos + 1, balance - 1)
        else return false
      }
      else
        return loop(pos + 1, balance)
    }

    loop(0, 0)
  }


  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {

    if (money == 0)
      return 1;
    else if (money < 0)
      return 0;
    else if (money >= 1 && coins.isEmpty)
      return 0;
    else
      return countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }

  def sum(f: Int => Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) return acc
      else loop(a + 1, acc + f(a))
    }
    loop(a, 0)
  }


  def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2

}
