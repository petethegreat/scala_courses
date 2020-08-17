package observatory

import org.apache.spark.{SparkConf, SparkContext}
import org.junit.Assert._
import org.junit.Test

trait ExtractionTest extends MilestoneSuite {
  private val milestoneTest = namedMilestoneTest("data extraction", 1) _

  // Implement tests for the methods of the `Extraction` object

  @Test def `getRDDfromResource 1979`: Unit = {
    val line = Extraction.getRDDFromResource("/1979.csv").first
    val expected = "010010,,01,01,2.1"
    assert(line == expected, s"expected ${expected}, actual ${line}")
  }

  @Test def `getRDDfromResource stations`: Unit = {
    val line = Extraction.getRDDFromResource("/stations.csv").first
    val expected = "007005,,,"
    assert(line == expected, s"expected ${expected}, actual ${line}")
  }

  @Test def `check convertStringTostationRecord`: Unit = {
    val line = "007018,,+00.000,+000.000"
    val expected = Extraction.stationRecord("007018".asInstanceOf[STN],"".asInstanceOf[WBAN],Some(0.0),Some(0.0) )
    val actual = Extraction.convertStringToStationRecord(line)
    assert(expected == actual, s"expected ${expected}, actual ${actual}")
  }

  @Test def `check convertStringToTempRecord`: Unit = {
    val line = "010010,,01,01,2.1"
    val expected = Extraction.temperatureRecord("010010".asInstanceOf[STN],"".asInstanceOf[WBAN],Some(1),Some(1),Some(2.1))
    val actual = Extraction.convertStringToTempRecord(line)
    assert(expected == actual, s"expected ${expected}, actual ${actual}")
  }

  @Test def `check conversion to station dataset` = {
    val statRDD = Extraction.getRDDFromResource("/stations.csv")
    val statDS = Extraction.stationDatasetFromRDD(statRDD)
//    statDS.printSchema()
    val expected = Extraction.stationRecord("007005".asInstanceOf[STN],"".asInstanceOf[WBAN],None,None)
    val actual = statDS.first
    assert(expected == actual, s"expected ${expected}, actual ${actual}")
  }
  @Test def `check conversion to temperature dataset 1979` = {
    val tempRDD = Extraction.getRDDFromResource("/1979.csv")
    val tempDS = Extraction.temperatureDatasetFromRDD(tempRDD)
    val expected = Extraction.temperatureRecord("010010".asInstanceOf[STN],"".asInstanceOf[WBAN],Some(1),Some(1), Some(2.1))
    // "010010,,01,01,2.1"
    val actual = tempDS.first
    assert(expected == actual, s"expected ${expected}, actual ${actual}")
  }


}
