package observatory

import com.sksamuel.scrimage.{Image, Pixel}

import scala.annotation.tailrec
import scala.math.{Pi, abs, acos, cos, sin, toRadians, pow}
import scala.util.Try


/**
  * 2nd milestone: basic visualization
  */
object Visualization extends VisualizationInterface {

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
    val TOLERANCE = 1.0e-9
  val closeEnough = 1/6400.0 // should be 1km
  val inverseDistanceP = 2.0


    def getLocationDifference(l1: Location, l2:Location): Double = {
//      return a delta sigma difference in radians
      //edge cases
      // assuming  -90 ≤ lat ≤ 90 and -180 ≤ lon ≤ 180
      if ((abs(l1.lat - l2.lat) < TOLERANCE) & (abs (l1.lon - l2.lon) < TOLERANCE) )
        0.0
      else if ((abs(l1.lat + l2.lat) < TOLERANCE ) & abs(abs(l1.lon - l2.lon) - 180.0) < TOLERANCE)
        Pi
      else
        acos( sin(l1.lat.toRadians)*sin(l2.lat.toRadians) + cos(l1.lat.toRadians)*cos(l2.lat.toRadians)*(l1.lon.toRadians - l2.lon.toRadians))
      }

    def getDeltaSigmas(temperatures: Iterable[(Location, Temperature)],location:Location) : Iterable[(Double, Temperature)] = {
//      compute difference in angles between locations
      temperatures.map( x => (getLocationDifference(x._1,location),x._2))
    }

  def aggDeltaSigmaTemp(dsigmaTemp: Iterable[(Double, Temperature)]): Temperature = {
    val inverseDistances = dsigmaTemp.map( k => (pow(k._1,-inverseDistanceP), k._2))
    val inverseDistanceSums = inverseDistances.foldLeft((0.0,0.0))( (uw_u, id_t) => (uw_u._1 + id_t._1 * id_t._2, uw_u._2 + id_t._1))
    inverseDistanceSums._1/inverseDistanceSums._2
  }

  def computeMeanTemperature(dSigmaTemp: Iterable[(Double, Temperature)]): Temperature = {
    // check if there is a zeroish distance observation

    val here = dSigmaTemp.collectFirst({case a if abs(a._1) < closeEnough => a._2})
    here match {
      case Some(temp) => temp
      case None => aggDeltaSigmaTemp(dSigmaTemp)
    }
  }


  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    val dSigmaTemp = getDeltaSigmas(temperatures,location)
    computeMeanTemperature(dSigmaTemp)
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
    def getColourPoints(points_sorted: Iterable[(Temperature, Color)], value: Temperature): ((Temperature, Color),(Temperature, Color))= {
      points match {
        case x::xs if value < x._1
        case x::y::ys if x._1 < value & y._1 > value => (x,y)

      }

    }
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {

    val lower = Try({points.filter(x => x._1 <= value).maxBy(_._1)}).toOption
    val upper = Try({points.filter(x => x._1 >= value).minBy(_._1)}).toOption

    (lower,upper) match { case Some((t1,c1)), Some((t2,c2)) => (c1 + c2)/2}



    lazy val sorted_points = points.sortBy(x => x._1)
    if (value <= points.head._1 ) points.head._2
//    if (value >= points.last._1) points.last._2
    points match {
      case x::y::ys  if
    }




  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {
    ???
  }

}

