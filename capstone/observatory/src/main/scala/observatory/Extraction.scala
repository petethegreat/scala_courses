package observatory

import java.time.LocalDate
import org.apache.spark.sql._
import org.apache.spark.sql.types._
import org.apache.spark.rdd.RDD
import org.apache.log4j.{Level, Logger}
import scala.io.Source
import scala.util.Try
import scala.collection.JavaConverters._
import org.apache.spark.sql.expressions.scalalang.typed
import org.apache.spark.sql.functions._


/**
  * 1st milestone: data extraction
  */
object Extraction extends ExtractionInterface {


  Logger.getLogger("org.apache.spark").setLevel(Level.WARN)


  val spark: SparkSession =
    SparkSession
      .builder()
      .appName("Time Usage")
      .master("local[4]")
      .config("spark.driver.memory","5g")
      .getOrCreate()

  // For implicit conversions like converting RDDs to DataFrames
  import spark.implicits._

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */


  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
    locateTemperaturesSpark(year, stationsFile, temperaturesFile)
  }

  def getRDDFromResource(path:String,npartitions:Int = 400):RDD[String] = {
    val lines = Source.fromInputStream(getClass.getResourceAsStream(path)).getLines.toStream
    spark.sparkContext.parallelize(lines,npartitions).map(x => x.toString)
  }

  //   Haven't used datasets much, so I'm gonna do that
  // some case classes to build datasets out of
  case class temperatureRecord(stationID: STN, WBANID: WBAN, month: Option[Int], day: Option[Int], tempF: Option[Double])
  case class stationRecord(stationID: STN, WBANID: WBAN, lat: Option[Double], lon: Option[Double])
  case class resultRecord(date: (Int,Int,Int),loc: Location,temp:Temperature)
  case class LocTempRecord(loc:Location, temp:Temperature)



  // Try - https://stackoverflow.com/a/23811475
  def convertStringToTempRecord(in:String): temperatureRecord = {
    val fields = in.split(',')
    temperatureRecord(
      Try(fields(0)).getOrElse("").asInstanceOf[STN],
      Try(fields(1)).getOrElse("").asInstanceOf[WBAN],
      Try(fields(2).toInt).toOption,
      Try(fields(3).toInt).toOption,
      Try(fields(4).toDouble).toOption)
  }

// Try - https://stackoverflow.com/a/23811475
  def convertStringToStationRecord(in:String): stationRecord = {
    val fields = in.split(',')
    stationRecord(
      Try(fields(0)).getOrElse("").asInstanceOf[STN],
      Try(fields(1)).getOrElse("").asInstanceOf[WBAN],
      Try(fields(2).toDouble).toOption,
      Try(fields(3).toDouble).toOption)
  }

  def temperatureDatasetFromRDD(input: RDD[String]): Dataset[temperatureRecord] = {
    input.map(convertStringToTempRecord).toDS
  }

  def stationDatasetFromRDD(input: RDD[String]): Dataset[stationRecord] = {
    input.map(convertStringToStationRecord).toDS.filter(col("lat").isNotNull && col("lon").isNotNull)
  }

  def joinTempStationDataSets(tempDS: Dataset[temperatureRecord], statDS: Dataset[stationRecord]): Dataset[(temperatureRecord,stationRecord)] = {
    tempDS.filter('stationID.isNotNull || 'WBANID.isNotNull).joinWith(statDS, tempDS("stationID") === statDS("stationID") && tempDS("WBANID") === statDS("WBANID"), "inner")
  }

  def convertFahrenheitToSensible(t:Double):Temperature = (t -32.0)*5.0/9.0

  def mapJoinedRecords(inDS: Dataset[(temperatureRecord,stationRecord)],year:Year) : Dataset[resultRecord] = {
    inDS.map{ case (temperatureRecord(a,b,Some(month), Some(day), Some(t)),stationRecord(c,d,Some(lat),Some(lon))) => resultRecord(
      (year,month,day),
      Location(lat,lon),
      convertFahrenheitToSensible(t)
    )}.as[resultRecord]
  }

//  https://stackoverflow.com/questions/43529513/how-to-convert-dataset-to-a-scala-iterable
def resultDStoIterable(inDS: Dataset[resultRecord]): Iterable[(LocalDate, Location, Temperature)] = {
  inDS.map(x => (x.date, x.loc, x.temp)).toLocalIterator.asScala.toIterable.map(x => (LocalDate.of(x._1._1,x._1._2,x._1._3),x._2,x._3))
}

  def locateTemperaturesSpark(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {

    // https://www.coursera.org/learn/scala-capstone/programming/NXfKi/scaffolding-material/discussions/threads/gOcSupeYROCnErqXmMTg2g
    // need to use getResourceAsStream(resource) to load the files as a filestream
    // then make rdd/dataset from that stream
    // then join

    // maybe some sort of global spark handler? singleton?
    // what does that solve? when would we call close()?
//    println("extraction locateTemperaturesSpark")
//    val npartitions = 400 - this is now defaullt

    val temp_ds = temperatureDatasetFromRDD(getRDDFromResource(temperaturesFile,600))
    val station_ds = stationDatasetFromRDD(getRDDFromResource(stationsFile,20))

    val joined = joinTempStationDataSets(temp_ds,station_ds)
    val result_DS = mapJoinedRecords(joined, year)
    resultDStoIterable(result_DS)
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
    def location_iterable_to_dataset(records: Iterable[(LocalDate, Location, Temperature)]): Dataset[LocTempRecord] = {
      spark.sparkContext.parallelize(records.toList).map(x => LocTempRecord(x._2,x._3)).toDS
    }
  def aggregateLocTempDS(inds:Dataset[LocTempRecord]): Dataset[LocTempRecord] = {
    inds.groupByKey(x => x.loc).agg(mean('temp).as[Temperature]).map(x => LocTempRecord(x._1,x._2)).as[LocTempRecord]
  }

  def locTempDStoIterable(inDS: Dataset[LocTempRecord]) : Iterable[(Location, Temperature)] = {
    inDS.map(x => (x.loc,x.temp)).toLocalIterator.asScala.toIterable
  }
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
    val ds = location_iterable_to_dataset(records)
    val aggregated = aggregateLocTempDS(ds)
    locTempDStoIterable(aggregated)
  }

}
