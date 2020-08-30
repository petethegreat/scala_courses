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
  val DEFAULTCOLOUR = Color(85,57,204) // blurple - xkcd


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

  def linearColourInterp(t1: Temperature, c1: Color, t2: Temperature, c2: Color, value: Temperature): Color = {
    val frac = (value - t1) / (t2 - t1)

    Color(
      ((1 - frac) * c1.red + frac * c2.red).toInt,
      ((1 - frac) * c1.green + frac * c2.green).toInt,
      ((1 - frac) * c1.blue + frac * c2.blue).toInt)
  }


  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {

    val lower = points.filter(x => x._1 <= value) match {
      case x if x.isEmpty => None
      case y => Some(y.maxBy( _._1))}

    val upper = points.filter(x => x._1 <= value) match {
      case x if x.isEmpty => None
      case y => Some(y.minBy( _._1))}


    (lower,upper) match {
      case (Some((t1,c1)), None) => c1
      case (None, Some((t1,c1))) => c1
      case (Some((t1,c1)), Some((t2,c2))) if (t1 == t2 ) => c1
      case (Some((t1,c1)), Some((t2,c2))) => linearColourInterp(t1, c1, t2, c2, value)
      case _ => DEFAULTCOLOUR
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

