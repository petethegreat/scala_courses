package reductions

import scala.annotation._
import org.scalameter._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime")
    println(s"speedup: ${seqtime.value / fjtime.value}")
  }
}

object ParallelParenthesesBalancing extends ParallelParenthesesBalancingInterface {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */

  def balance(chars: Array[Char]): Boolean =
  {
    val cum_opens = chars
      .map(x =>  if (x == '(') 1 else if (x==')') -1 else 0 )
      .filter(x => x != 0)
      .scanLeft(0)((a,b) => a + b)
    if (cum_opens.last != 0 | cum_opens.exists(x => x <0) )
      false
    else
      true
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, delta: Int, min: Int) /*: ???*/ = {
      //traverse counts the difference ( opens minus close), as well as the min difference
      // ((())) is balanced
      // )))((( is not
      val i = idx
      while (i < until) {

      }
    }

    def reduce(from: Int, until: Int) /*: ???*/ = {
      if from()
    }

    reduce(0, chars.length) == ???
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
