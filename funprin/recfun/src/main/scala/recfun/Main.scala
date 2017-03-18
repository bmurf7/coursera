package recfun

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {

    /*
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    */
    val money = 0
    val n = countChange(money, List(1,5,10))
    println(s"countChange($money,List(1,5,10) = $n")
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int =
      if (c == 0 || c == r) 1 else pascal(c-1,r-1) + pascal(c,r-1)

  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def cnt(c: Char): Int = c match {
        case '(' => 1
        case ')' => -1
        case _ => 0
      }
      @tailrec
      def balanceIter(acc: Int, chars: List[Char]): Boolean = {
        if (chars.isEmpty || acc < 0)  acc == 0
        else balanceIter(acc + cnt(chars.head), chars.tail)
      }
      balanceIter(0,chars)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {

      def countChangeIter(money: Int, coins: List[Int]): Int = {
        if (money < 0) 0
        else if (money == 0) 1
        else if (coins.isEmpty) 0
        else {
          countChangeIter(money - coins.head, coins) + countChangeIter(money, coins.tail)
        }
      }
      if (money == 0 || coins.isEmpty) 0
      else countChangeIter(money,coins.sorted.reverse)
    }
  }
