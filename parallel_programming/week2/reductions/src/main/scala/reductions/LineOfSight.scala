package reductions

import org.scalameter._

object LineOfSightRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 100,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 10000000
    val input = (0 until length).map(_ % 100 * 1.0f).toArray
    val output = new Array[Float](length + 1)
    val blanklines = 12
    val seqtime = standardConfig measure {
      LineOfSight.lineOfSight(input, output)
    }
    var ii =0
    while (ii < blanklines) {
      println("-")
      ii += 1
    }

    println(s"sequential time: $seqtime")

    val partime = standardConfig measure {
      LineOfSight.parLineOfSight(input, output, 10000)
    }
    ii =0
    while (ii < blanklines) {
      println("-")
      ii += 1
    }
    println(s"parallel time: $partime")
    println(s"speedup: ${seqtime.value / partime.value}")
  }
}

sealed abstract class Tree {
  def maxPrevious: Float
}

case class Node(left: Tree, right: Tree) extends Tree {
  val maxPrevious = left.maxPrevious.max(right.maxPrevious)
}

case class Leaf(from: Int, until: Int, maxPrevious: Float) extends Tree

object LineOfSight extends LineOfSightInterface {

  def lineOfSight(input: Array[Float], output: Array[Float]): Unit = {
    var max: Float = 0
    var ii = 1
    output(0) = 0
    while (ii < input.length) {
      val grad = input(ii)/ii
      if (grad > max) max = grad
      output(ii) = math.max(grad,max)
      ii += 1
    }

  }

  /** Traverses the specified part of the array and returns the maximum angle.
   */
  def upsweepSequential(input: Array[Float], from: Int, until: Int): Float = {
    var max: Float = 0
    var ii = from
    while (ii < until){
      val grad = input(ii)/ii
      max = math.max(max,grad)
      ii += 1
    }
    max
  }

  /** Traverses the part of the array starting at `from` and until `end`, and
   *  returns the reduction tree for that part of the array.
   *
   *  The reduction tree is a `Leaf` if the length of the specified part of the
   *  array is smaller or equal to `threshold`, and a `Node` otherwise.
   *  If the specified part of the array is longer than `threshold`, then the
   *  work is divided and done recursively in parallel.
   */
  def upsweep(input: Array[Float], from: Int, end: Int,
    threshold: Int): Tree = {
    if (from == 0) upsweep(input,1,end,threshold)
    else if (end - from <= threshold) Leaf(from,end,upsweepSequential(input,from,end))
    else {
      val mid = from + (end - from)/2
      val (tL,tR) = parallel(
        upsweep(input,from,mid,threshold),
        upsweep(input,mid,end,threshold))
      Node(tL,tR)
    }
  }

  /** Traverses the part of the `input` array starting at `from` and until
   *  `until`, and computes the maximum angle for each entry of the output array,
   *  given the `startingAngle`.
   */
  def downsweepSequential(input: Array[Float], output: Array[Float],
    startingAngle: Float, from: Int, until: Int): Unit = {
    var ii = from
    var maxAngle = startingAngle
    while (ii < until){
      val thisAngle = input(ii)/ii
      maxAngle = math.max(thisAngle,maxAngle)
      output(ii) = maxAngle
      ii += 1
    }
  }

  /** Pushes the maximum angle in the prefix of the array to each leaf of the
   *  reduction `tree` in parallel, and then calls `downsweepSequential` to write
   *  the `output` angles.
   */
  def downsweep(input: Array[Float], output: Array[Float], startingAngle: Float,
    tree: Tree): Unit = tree match {
    case Leaf(from,until,_) => downsweepSequential(input,output,startingAngle,from,until)
    case Node(left,right) => parallel(
      downsweep(input,output,startingAngle,left),
      downsweep(input,output,left.maxPrevious,right))

    }



  /** Compute the line-of-sight in parallel. */
  def parLineOfSight(input: Array[Float], output: Array[Float],
    threshold: Int): Unit = {
    val resTree = upsweep(input,0,input.length,threshold)
    downsweep(input,output,0,resTree)
  }
}
