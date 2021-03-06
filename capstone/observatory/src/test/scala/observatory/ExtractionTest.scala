package observatory

import java.time.LocalDate

import org.apache.spark.{SparkConf, SparkContext}
import org.junit.Assert._
import org.apache.spark.sql._
import org.junit.Test
import org.apache.spark.sql.functions._
import Extraction.spark.implicits._


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
    statDS.show(10,false)
    val expected = Extraction.stationRecord("007018".asInstanceOf[STN],"".asInstanceOf[WBAN],Some(0.0),Some(0.0))
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

  @Test def `check dataset join` = {
    val statlines = Seq(
      "008268,,+32.950,+065.567",
      "008269,,+32.950,+063.567",
      "008270,,+32.950,+066.567"
    )
    val templines = Seq(
      "008268,,01,07,26.4",
      "008269,,01,07,26.5",
      "008269,,01,08,26.6"
    )
    val tempDS = Extraction.temperatureDatasetFromRDD(Extraction.spark.sparkContext.parallelize(templines))
    val statDS = Extraction.stationDatasetFromRDD(Extraction.spark.sparkContext.parallelize(statlines))

    val actual1 = Extraction.joinTempStationDataSets(tempDS,statDS).collect()
//    actual1.foreach(println)
    val actual2 = actual1.groupBy(x => x._1.stationID).mapValues(x => x.size)
    val actual3 = actual2.toSeq.sortBy(x => x._1).toList
    val expected = Seq(("008268",1),("008269",2))
    assert( actual3 == expected,s"joined result count: $actual3, expected: $expected")
  }
  @Test def `check dataset join mapped to resultRecord` = {
    val results = Seq(
      (Extraction.temperatureRecord("moose", "duck", Some(2), Some(20), Some(32.0)), Extraction.stationRecord("moose", "duck", Some(43.660512), Some(-79.398082))),
      (Extraction.temperatureRecord("moose", "duck", Some(8), Some(2), Some(65.0)), Extraction.stationRecord("moose", "duck", Some(43.660512), Some(-79.398082)))
    )

    val rds = Extraction.spark.sparkContext.parallelize(results).toDS

    val resultrecordds = Extraction.mapJoinedRecords(rds, 1979)
    resultrecordds.collect.foreach(println)
  }

  @Test def `check dataset to iterable` = {
    val resultrecords = Seq(
      Extraction.resultRecord((2019,8,2), Location(23.5,26.2), 27.2),
      Extraction.resultRecord((2019,8,2), Location(23.6,26.1), 27.5),
      Extraction.resultRecord((2019,8,2), Location(23.7,26.0), 36.4)
    )
    val res_ds = Extraction.spark.sparkContext.parallelize(resultrecords).toDS

    val local = Extraction.resultDStoIterable(res_ds)

    local.foreach(println)
    val actual = local.map(x => x._3).toList.sorted
    val expected = List[Temperature](27.2,27.5,36.4)

    assert(actual == expected, s"expected ${expected}, actual ${actual}")
  }

//  @Test def `convert iterable to DS` = {
//    val theIterable = Iterable(
//      (LocalDate.of(2019,8,2), Location(23.5,26.2), 27.2),
//      (LocalDate.of(2019,8,2), Location(23.6,26.1), 27.5),
//      (LocalDate.of(2019,8,2), Location(23.7,26.0), 36.4)
//    )
//    val result = Extraction.location_iterable_to_dataset(theIterable)
//    result.show(20,false)
//
//  }

  @Test def `aggregateLocTemp DS` = {
    val theIterable = Iterable(
      (LocalDate.of(2019,8,2), Location(23.5,26.1), 27.2),
      (LocalDate.of(2019,8,2), Location(23.5,26.1), 27.8),
      (LocalDate.of(2019,8,2), Location(23.7,26.0), 36.2),
      (LocalDate.of(2019,8,2), Location(23.7,26.0), 36.8)

    )
    val locTempDS = Extraction.location_iterable_to_dataset(theIterable)
    val actual = Extraction.aggregateLocTempDS(locTempDS).orderBy('temp).collect()
    val expected = Array(Extraction.LocTempRecord(Location(23.5,26.1),27.5), Extraction.LocTempRecord(Location(23.7,26.0),36.5))
    // this is checking for floating point equality, which is sketchball
    assert(expected.zip(actual).forall(x => x._1 == x._2), s"expected ${expected.mkString(",")}, actual ${actual.mkString(",")}")
  }

  @Test def `test average yearly` = {
    val theIterable = Iterable(
      (LocalDate.of(2019,8,2), Location(23.5,26.1), 27.2),
      (LocalDate.of(2019,8,2), Location(23.5,26.1), 27.8),
      (LocalDate.of(2019,8,2), Location(23.7,26.0), 36.2),
      (LocalDate.of(2019,8,2), Location(23.7,26.0), 36.8)
    )
    val result = Extraction.locationYearlyAverageRecords(theIterable)
    val expected = Iterable((Location(23.5,26.1),27.5), (Location(23.7,26.0),36.5))
    assert(expected.zip(result).forall(x => x._1 == x._2), s"expected ${expected.mkString(",")}, result ${result.mkString(",")}")
  }


}


