
import scala.math._

def sumSegment(a: Array[Int], p:Double, s: Int, t:Int): Int =
  {
    def raise(aa:Int): Int = floor(pow(aa.abs,p)).toInt

    a.slice(s,t).map(raise).sum
  }

val myArray = Array(1,2,3,4,5,6)

sumSegment(myArray,2,0,3)

