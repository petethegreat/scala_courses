package reductions

import org.scalameter._

object ParallelCountChangeRunner {

  @volatile var seqResult = 0

  @volatile var parResult = 0

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 80,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val amount = 250
    val coins = List(1, 2, 5, 10, 20, 50)
    val seqtime = standardConfig measure {
      seqResult = ParallelCountChange.countChange(amount, coins)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential count time: $seqtime")

    def measureParallelCountChange(threshold: => ParallelCountChange.Threshold): Unit = try {
      val fjtime = standardConfig measure {
        parResult = ParallelCountChange.parCountChange(amount, coins, threshold)
      }
      println(s"parallel result = $parResult")
      println(s"parallel count time: $fjtime")
      println(s"speedup: ${seqtime.value / fjtime.value}")
    } catch {
      case e: NotImplementedError =>
        println("Not implemented.")
    }

    println("\n# Using moneyThreshold\n")
    measureParallelCountChange(ParallelCountChange.moneyThreshold(amount))
    println("\n# Using totalCoinsThreshold\n")
    measureParallelCountChange(ParallelCountChange.totalCoinsThreshold(coins.length))
    println("\n# Using combinedThreshold\n")
    measureParallelCountChange(ParallelCountChange.combinedThreshold(amount, coins))
  }
}

object ParallelCountChange extends ParallelCountChangeInterface {

  /** Returns the number of ways change can be made from the specified list of
   *  coins for the specified amount of money.
   */
  def countChange(money: Int, coins: List[Int]): Int = {
      // function for recursion
      //@scala.annotation.tailrec
      def coinNumber(money: Int, coins: List[Int], combinations: Int): Int =
      {
        //termination conditions
        // we've made our change
        if (money == 0) {
          //println("combination works")
          combinations +1}
        // we're out of coins, with money remaining
        else if (coins.isEmpty) {
          //println("combination does not work")
          combinations}
        // if we have money, use more coins
        else if (money > 0 ) {
          //println(s"money $money - trying  $coins.head")
          // use one (more) of this coin, and try without any more of this coin
          coinNumber(money - coins.head,coins,combinations) + coinNumber(money ,coins.tail,combinations)
        } else 0 // we can't use any more of this coin - not going to get any more use out of it
      }
      // end definition of coinNumber
      coinNumber(money,coins,0)
    }



  type Threshold = (Int, List[Int]) => Boolean

  /** In parallel, counts the number of ways change can be made from the
   *  specified list of coins for the specified amount of money.
   */
  def parCountChange(money: Int, coins: List[Int], threshold: Threshold): Int = {
    if (money == 0) 1
    else if (coins.isEmpty | money < 0) 0
    else if (threshold(money, coins)) countChange(money, coins)
    else {
      val x = parallel(parCountChange(money - coins.head, coins, threshold), parCountChange(money, coins.tail, threshold))
      x._1 + x._2
    }
  }


  /** Threshold heuristic based on the starting money. */
  def moneyThreshold(startingMoney: Int): Threshold =
    (x:Int, y:List[Int]) => x < 2*startingMoney/3

  /** Threshold heuristic based on the total number of initial coins. */
  def totalCoinsThreshold(totalCoins: Int): Threshold =
    (x:Int, y:List[Int]) => y.length <= 2*totalCoins/3


  /** Threshold heuristic based on the starting money and the initial list of coins. */
  def combinedThreshold(startingMoney: Int, allCoins: List[Int]): Threshold = {
    (x:Int, y:List[Int]) => 2*x*y.length < startingMoney*allCoins.length
  }
}
