package observatory

import org.apache.spark.{SparkConf, SparkContext}
import org.junit.Assert._
import org.junit.Test

trait ExtractionTest extends MilestoneSuite {
  private val milestoneTest = namedMilestoneTest("data extraction", 1) _

  // Implement tests for the methods of the `Extraction` object

  @Test def `getRDDfromResource`: Unit = {
    val line = Extraction.getRDDFromResource("/1979.csv").first
    val expected = "010010,,01,01,2.1"
    assert(line == expected, s"expected ${expected}, actual ${line}")
  }
  
}
