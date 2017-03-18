package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer new Warmer.Default

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def cnt(c: Char): Int = c match {
      case '(' => 1
      case ')' => -1
      case _ => 0
    }
    @tailrec
    def balanceIter(acc: Int, chars: Array[Char]): Boolean = {
      if (chars.isEmpty || acc < 0)  acc == 0
      else balanceIter(acc + cnt(chars.head), chars.tail)
    }
    balanceIter(0,chars)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    // reduction operator
    def fop(c: Char, first: Int, total: Int): (Int, Int) = {
      def cnt(c: Char): Int = c match {
        case '(' => 1
        case ')' => -1
        case _ => 0
      }
      val current = cnt(c)
      val f = if (first == 0) current else first
      (f, total + current)
    }
    // sequential traversal
    // use a while loop, or make tail-recursive -- do not use a Range
    def traverse(idx: Int, until: Int, first: Int, total: Int): (Int, Int)  = {
      var f = first
      var t = total
      var i = idx
      while (i < until) {
        val (f1,t1) = fop(chars(i), f, t)
        f = f1; t = t1
        i = i + 1
      }
      (f, t)
    }

    // parallel reduction
    def reduce(from: Int, until: Int): (Int, Int)  = {
      var first = 0
      var total = 0
      if (until - from <= threshold) {
        val (f, t) = traverse(from, until, first, total)
        (f,t)
      } else {
        val mid = from + (until - from)/2
        val ((lF,lT),(rF,rT)) = parallel(reduce(from, mid),
                               reduce(mid, until))
        (lF, lT + rT)
      }
    }

    //println(s"parBalance: chars[%s] len=${chars.length} threshold=$threshold".format(chars.mkString("")))
    reduce(0, chars.length) == (1, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
