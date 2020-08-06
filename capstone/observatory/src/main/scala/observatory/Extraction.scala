package observatory

import java.time.LocalDate
import org.apache.spark.sql._
import org.apache.spark.sql.types._
import org.apache.log4j.{Level, Logger}

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

  def locateTemperaturesSpark(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {

    // https://www.coursera.org/learn/scala-capstone/programming/NXfKi/scaffolding-material/discussions/threads/gOcSupeYROCnErqXmMTg2g
    // need to use getResourceAsStream(resource) to load the files as a filestream
    // then make rdd/dataset from that stream
    // then join

    // maybe some sort of global spark handler? singleton?
    // what does that solve? when would we call close()?

  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
    ???
  }

}
