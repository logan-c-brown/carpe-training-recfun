package recfun

import scala.annotation.tailrec

object Main{
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    println
    println
    println(countChange(5, List(1,2,3)))
    println(countChange(6, List(1,2)))
    println(countChange(6, List(1)))
    println(balance("(if (zero? x) max (/ 1 x))".toList))
    println(balance("I told him (that it’s not (yet) done). (But he wasn’t listening)".toList))
    println(balance(":-)".toList))
    println(balance("())(".toList))
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      (c,r) match {
        case (0, _) => 1 // first column of the matrix
        case (i, j) if i == j => 1 // diagonal on the matrix
        case (n, m) => pascal(n, m-1) + pascal(n-1, m-1) // the node in matrix is calculated from one directly above, and the one to the upper left
      }

    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      val relevantData = chars.filter(List('(',')').contains(_)).mkString
      def rmGoodPaths(s: String): String = {
        val pathRemoved = s.replaceAllLiterally("()","")
        pathRemoved match {
          case "" | `s` => pathRemoved
          case _: String => rmGoodPaths(pathRemoved)
        }
      }

      rmGoodPaths(relevantData).isBlank
    }


  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      (money, coins) match {
        case (0, _) => 1 // success, the cashier gave away the right amount of coins
        case (_, Nil) => 0 // there are no coins to process, the till does not have the required coin to give change
        case (m, _) if m < 0 => 0 // 0 means a failure, where the coin was too big and caused the cashier to give away free money
        case (m, coin :: rest) => countChange(m - coin, coin :: rest) + countChange(m, rest)
      }
    }

  main(Array())
  }
