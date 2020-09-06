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
        acos( sin(l1.lat.toRadians)*sin(l2.lat.toRadians) + cos(l1.lat.toRadians)*cos(l2.lat.toRadians)*cos(l1.lon.toRadians - l2.lon.toRadians))
      }

    def getDeltaSigmas(temperatures: Iterable[(Location, Temperature)],location:Location) : Iterable[(Double, Temperature)] = {
//      compute difference in angles between locations
      temperatures.map( x => (getLocationDifference(x._1,location),x._2))
    }

  def aggDeltaSigmaTemp(dsigmaTemp: Iterable[(Double, Temperature)]): Temperature = {
    val inverseDistances = dsigmaTemp.map( k => (pow(k._1,-inverseDistanceP), k._2))
//    id_t - (inverse distance, temperature) tuple
    // uw_u._1 - sum of inverse_distance * temp
    // uw_u._2 - sum of inverse_distance

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
      ((1.0 - frac) * c1.red + frac * c2.red).round.toInt,
      ((1.0 - frac) * c1.green + frac * c2.green).round.toInt,
      ((1.0 - frac) * c1.blue + frac * c2.blue).round.toInt)
//    Color(
//      (((value - t1)* c1.red +   (t2-value)*c2.red)/(t2-t1)).round.toInt,
//      (((value - t1)* c1.green +   (t2-value)*c2.green)/(t2-t1)).round.toInt,
//      (((value - t1)* c1.blue +   (t2-value)*c2.blue)/(t2-t1)).round.toInt)
  }


  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {

//    var lower: Option[(Temperature,Color)] = None
//    var upper: Option[(Temperature,Color)] = None
//    for (tc <- points) {
//
//    }
    val lower = points.filter(x => x._1 <= value) match {
      case x if x.isEmpty => None
      case y => Some(y.maxBy( _._1))}

    val upper = points.filter(x => x._1 >= value) match {
      case x if x.isEmpty => None
      case y => Some(y.minBy( _._1))}


    (lower,upper) match {
      case (Some((t1,c1)), None) => c1
      case (None, Some((t1,c1))) => c1
      case (Some((t1,c1)), Some((t2,c2))) if (t1 == t2 ) => c1
      case (Some((t1,c1)), Some((t2,c2))) => linearColourInterp(t1, c1, t2, c2, value)
      case(None,None) => Color(0,0,0) // This seems to happen
      case _ => DEFAULTCOLOUR
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
    def getPixLocations(nPix: (Int,Int), LatDims: (Double,Double), LonDims: (Double,Double)): (Seq[Double], Seq[Double]) = {
      // take number of pixels and lat/long boundaries, return iterables of lat/lon coordinates
      // npix is in x/y, so nPix._1 corresponds to lon dimension
      val dlon = (LonDims._2 - LonDims._1) /(nPix._1 -1)
      val dlat = (LatDims._2 - LatDims._1) /(nPix._2 -1)

      val LonVals = Seq.iterate[Double](LonDims._1, nPix._1)(_ + dlon)
      val LatVals = Seq.iterate[Double](LatDims._1, nPix._2)(_ + dlat)

      (LatVals,LonVals)
    }
def getDefaultColours(): Iterable[(Temperature, Color)] = {
  Seq(
    (-60.0, Color(0, 0, 0)),
    (-50.0, Color(33, 0, 107)),
    (-27.0, Color(255, 0, 255)),
    (-15.0, Color(0, 0, 255)),
    (0.0, Color(0, 255, 255)),
    (12.0, Color(255, 255, 0)),
    (32.0, Color(255, 0, 0)),
    (60.0, Color(255, 255, 255))
  )

}
def getHiTempRangeColours(): Iterable[(Temperature, Color)] = {
  Seq(
    (-100.0, Color(255, 0, 0)),
    (0.0, Color(255, 255, 255)),
    (100.0, Color(0, 0, 255))
  )

}

  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {
    val nPix = (360, 180)
//    val nPix = (720, 360)
    val lat_dims = (-90.0,89.0)
    val lon_dims = (-180.0,179.0)

    val (lats, lons) = getPixLocations(nPix, lat_dims, lon_dims)

    val arr_pix = new Array[Pixel](nPix._1*nPix._2)
    // initialise to white
    for (ix <- 0 until nPix._1;
    iy <- 0 until nPix._2) arr_pix(ix +iy*nPix._1) = Pixel(255,255,255,255)


    // parallelise on lat dimension, as we move across lon
    // as we move across x memory is adjacent. (lon)
    // parallelise across lat (y)

    val ix_lon = lons.zipWithIndex
    val iy_lat = lats.reverse.zipWithIndex


    iy_lat.foreach( iy =>  for (jx <- ix_lon){
      val temp = predictTemperature(temperatures, Location(iy._1, jx._1))
      val cc = interpolateColor(colors, temp)
      arr_pix(jx._2 + nPix._1*iy._2) = Pixel(cc.red, cc.green, cc.blue,255)
    })

    Image(nPix._1, nPix._2, arr_pix)
  }

  def WriteTestImage():Unit = {

    val outPath = "target/test_image.png"
    val temps = Seq(
      (Location(90.0,0.0), 60.0),
      (Location(-90.0,0.0), 45.0),
      (Location(10, 170),-60.0),
//      (Location(-10,-170),-45.0),
      (Location(0,0),0.0)
    )
    val colours = getHiTempRangeColours() // getDefaultColours()
    val image = visualize(temps,colours)

    // write
    image.output(new java.io.File(outPath))
  }

}


