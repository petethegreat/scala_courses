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

  @Test def `check convertStringTostationRecord`: Unit = {
    val line = "007018,,+00.000,+000.000"
    val expected = Extraction.stationRecord("007018".asInstanceOf[STN],"".asInstanceOf[WBAN],0.0,0.0 )
    val actual = Extraction.convertStringToStationRecord(line)
    assert(expected == actual, s"expected ${expected}, actual ${actual}")
  }

  @Test def `check convertStringToTempRecord`: Unit = {
    val line = "010010,,01,01,2.1"
    val expected = Extraction.temperatureRecord("010010".asInstanceOf[STN],"".asInstanceOf[WBAN],"01".toInt,"01".toInt,"2.1".toDouble)
    val actual = Extraction.convertStringToTempRecord(line)
    assert(expected == actual, s"expected ${expected}, actual ${actual}")
  }
}
