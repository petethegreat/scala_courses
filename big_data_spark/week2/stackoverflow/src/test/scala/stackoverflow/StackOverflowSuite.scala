package stackoverflow

import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.rdd.RDD
import org.junit._
import org.junit.Assert.assertEquals
import java.io.File

object StackOverflowSuite {
  val conf: SparkConf = new SparkConf().setMaster("local[4]").setAppName("StackOverflow")
  val sc: SparkContext = new SparkContext(conf)
}

class StackOverflowSuite {
  import StackOverflowSuite._


  lazy val testObject = new StackOverflow {
    override val langs =
      List(
        "JavaScript", "Java", "PHP", "Python", "C#", "C++", "Ruby", "CSS",
        "Objective-C", "Perl", "Scala", "Haskell", "MATLAB", "Clojure", "Groovy")
    override def langSpread = 50000
    override def kmeansKernels = 45
    override def kmeansEta: Double = 20.0D
    override def kmeansMaxIterations = 120
  }

  @Test def `testObject can be instantiated`: Unit = {
    val instantiatable = try {
      testObject
      true
    } catch {
      case _: Throwable => false
    }
    assert(instantiatable, "Can't instantiate a StackOverflow object")
  }

  @Test def `check_data_load`: Unit = {
    val lines = sc.textFile("src/main/resources/stackoverflow/stackoverflow.csv").take(4)
    lines.foreach(println)


    assert(lines.length == 4, "could not read lines")
  }

  @Test def `check_scored_postings`: Unit = {
    val lines = sc.textFile("src/main/resources/stackoverflow/stackoverflow.csv")
    val raw     = testObject.rawPostings(lines)
    val grouped = testObject.groupedPostings(raw)
    val scored  = testObject.scoredPostings(grouped)
//    val ordered  = scored.filter(_._1.id < 200).take(5)
    val ordered  = scored.filter(x => List(6,42,72,126,174).contains(x._1.id ))
//    val ordered  = scored.map(x => (x._1.id,x)).sortByKey(true).take(5).map(x => x._2)
    ordered.foreach(println)
// assert that these are contained
//    (Posting(1,126,None,None,33,Some(Java)),30)
//    (Posting(1,6,None,None,140,Some(CSS)),67)
//    (Posting(1,42,None,None,155,Some(PHP)),89)
//    (Posting(1,72,None,None,16,Some(Ruby)),3)
//    (Posting(1,174,None,None,38,Some(C#)),20)



//    assert(lines.length == 4, "could not read lines")
  }


  @Rule def individualTestTimeout = new org.junit.rules.Timeout(100 * 1000)
}
