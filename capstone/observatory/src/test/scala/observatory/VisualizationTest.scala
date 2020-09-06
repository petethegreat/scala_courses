package observatory

import org.junit.Assert._
import org.junit.Test

import math.abs

trait VisualizationTest extends MilestoneSuite {
  private val milestoneTest = namedMilestoneTest("raw data display", 2) _

  val TOLERANCE = 1.0e-9

  // Implement tests for the methods of the `Visualization` object
@Test def `visualisation: check deltasigma same` = {
  val l1 = Location(87.0,36.5)
  val expected = 0.0
  val actual = Visualization.getLocationDifference(l1,l1)
  assert(expected == actual, s"expected ${expected}, actual ${actual}")
}
  @Test def `visualisation: check deltasigma antipodes` = {
    val l1 = Location(87.0,-90.5)
    val l2 = Location(-87.0,89.5)
    val expected = math.Pi
    val actual = Visualization.getLocationDifference(l1,l2)
    assert(expected == actual, s"expected ${expected}, actual ${actual}")
  }

  @Test def `visualisation: check deltasigma other` = {
    val l1 = Location(90.0,36.5)
    val l2 = Location(80.0,216.5)
    val expected = 0.17453292519943317
    val actual = Visualization.getLocationDifference(l1,l2)
    assert(abs(expected - actual) < TOLERANCE, s"expected ${expected}, actual ${actual}")
  }

  @Test def `visualisation: aggDeltaSigmaTemp sums correctly` = {
    val dsigma_temp:Iterable[(Double,Temperature)] = Iterable( (1.0,40.0), (2.0,50.0))
    val actual = Visualization.aggDeltaSigmaTemp(dsigma_temp)
    val expected = 42.0
    assert(abs(actual - expected) < TOLERANCE, s"expected ${expected}, actual ${actual}")
  }

  @Test def `visualisation: computeMeanTemperature returns zero distance result` = {
    val observations = Iterable(
      (3.0, 3.0),
      (0.5, 9.7),
      (1.1, 19.7),
      (2.0e-4, 99.7),
      (1.0e-4, 36.5)
    )

    val expected = 36.5
    val actual = Visualization.computeMeanTemperature(observations)
    assert(abs(actual - expected) < TOLERANCE, s"expected ${expected}, actual ${actual}")
  }

  @Test def `visualisation: check predictTemperature` = {
    val observations = Iterable(
      (Location(2.0, 90.0), 38.5),
      (Location(-2.0, 90.0), 36.5),
      (Location(3.0, 90.0), 40.0),
      (Location(-3.0, 90.0), 35.0)
    )

    val ref_loc =  Location(0.0, 90.0)

    val actual = Visualization.predictTemperature(observations, ref_loc)
    val expected = 37.5
    assert(abs(actual - expected) < TOLERANCE, s"expected ${expected}, actual ${actual}")
  }
  @Test def `visualisation: check predictTemperature again` = {

    // for a given set of observations, make sure we can get predictions at all locations
    val temps = Seq(
      (Location(43.650381, -79.417962), 45.0),
      (Location(-22.412246, 132.394754), 73.3),
      (Location(-45.763782, 170.317367),-4.2),
      (Location(-45.218830, 169.354580),37.5)
    )

    val min_t = temps.minBy(_._2)._2
    val max_t = temps.maxBy(_._2)._2
    println(min_t)
    println(max_t)
    val nPix = (720, 360)
    val lat_dims = (-90.0,89.0)
    val lon_dims = (-180.0,179.0)

    val (lats, lons) = Visualization.getPixLocations(nPix, lat_dims, lon_dims)

    val locations = for (lat <- lats; lon <- lons) yield Location(lat,lon)

    val predictions = locations.map(Visualization.predictTemperature(temps, _))

    val bads = locations.zip(predictions).filter(x => (x._2 < min_t) || (x._2 > max_t))
    println(s"bads length: ${bads.size}")
    bads.take(20).foreach(println)

    val bads2 = predictions.filter(x => (x < min_t) || (x > max_t))
    println(s"bads2 length: ${bads2.size}")
    bads2.take(20).foreach(println)

    val exceeds = predictions.exists(_ > max_t)
    val toolow = predictions.exists(_ < min_t)

    println(s"exceeds: ${exceeds}, toolow: ${toolow}")



    assert(predictions.forall( x => ((x >= min_t) && (x <= max_t) )), s"obtained prediction out of bounds")
  }


  @Test def `visualisation: getPixelValues correct lat/lon values`: Unit = {

    val nPix = (360, 180)
    val lat_dims = (-90.0,89.0)
    val lon_dims = (-180.0,179.0)

    val lat_expected = Seq.iterate[Double](lat_dims._1, nPix._2)(_ + 1.0)
    val lon_expected = Seq.iterate[Double](lon_dims._1, nPix._1)(_ + 1.0)

    val (lat_actual, lon_actual) = Visualization.getPixLocations(nPix, lat_dims, lon_dims)

//    println(s"lat size: ${lat_actual.size}")
//    println(lat_actual)
//
//    println(s"\nlon size: ${lon_actual.size}")
//    println(lon_actual)

    assert ((lat_actual == lat_expected) & (lon_actual == lon_expected), s"expected and actual lat/lon values differ")
  }
  @Test def `visualisation: check color interpolation`: Unit = {

    val colours = Visualization.getDefaultColours()
    val inputTemps = Seq(-70.0,-60.0,-55.0, -50.0,-27.0,22.0,60.0,70.0)
//    val results = inputTemps.map(x => (x, Visualization.interpolateColor(colours,x)))
//    def colourDiff(c1: Color, c2:Color): Double = abs(c1.red - c2.red) + abs(c1.green - c2.green) + abs(c1.blue - c2.blue)
    val expected = Seq(
      Color(0,0,0),
      Color(0,0,0),
      Color(17,0,54),
      Color(33,0,107),
      Color(255,0,255),
      Color(255,128,0),
      Color(255,255,255),
      Color(255,255,255))
    val actual = inputTemps.map(Visualization.interpolateColor(colours,_))
    val different = actual.zip(expected).filter(x => x._1 != x._2).foreach(println)
    assert(expected == actual, "interpolated colours differ from expected")

//    println()
//    println("colour interpolation results")
//    results.foreach(println)
  }

  @Test def `visualisation: check color interpolation 2`: Unit = {

    val colours:Seq[(Temperature,Color)] = Seq(
      (0.0,Color(0,0,0)),
      (100.0,Color(100,100,100)))

    val test_temps:Seq[Temperature] = (0 to 10).map(x => x*10.0)
    val actual = test_temps.map( Visualization.interpolateColor(colours,_))
    val expected = (0 to 10).map(x => Color(x*10,x*10,x*10)).toSeq
    actual.zip(expected).foreach(println)

    assert(expected == actual, "interpolated colours differ from expected")
  }


@Test def `visualisation: check dsigma for nans`: Unit = {
  val ref_loc = Location(0,90)

  val nPix = (360, 180)
  val lat_dims = (-90.0,89.0)
  val lon_dims = (-180.0,179.0)

  val (lats,lons) = Visualization.getPixLocations(nPix, lat_dims, lon_dims)

  val locations = for (lat <- lats; lon <- lons) yield Location(lat,lon)

  val dsigmas = locations.map(Visualization.getLocationDifference(_,ref_loc))

  val nans = dsigmas.filter(x => x.isNaN).size

  assert(nans == 0,s" expected 0 nans, found ${nans} nans (of ${dsigmas.size}")

}

  @Test def `check dsigma`:Unit = {
    val diff_tuples = Seq(
      (Location(0.0,0.0),Location(10.0,0.0)),
      (Location(0.0,0.0),Location(20.0,0.0)),
      (Location(0.0,0.0),Location(45.0,0.0)),
      (Location(90.0,0.0),Location(0.0,17.0))

    )
    val expected_diff_rad = Seq(
      10.0.toRadians,
      20.0.toRadians,
      math.Pi/4.0,
      math.Pi/2.0
    )

    val actual = diff_tuples.map(x => Visualization.getLocationDifference(x._1,x._2))

    val comparison = actual.zip(expected_diff_rad)
//    diff_tuples.zip(comparison).filter(x => abs(x._2._1 - x._2._2) > TOLERANCE).foreach(println)
    diff_tuples.zip(comparison).foreach(println)


    assert(actual.zip(expected_diff_rad).forall(x => abs(x._1 - x._2) <= TOLERANCE), "visualisation: expected and actual dsigma values do not agree")
  }

@Test def `Visualisation check 1976 colour`: Unit = {

  //  [Test Description] visualize (5pts)(observatory.CapstoneSuite)
  //  [Observed Error] Incorrect computed color at Location(-33.0,-158.0): Color(125,0,130). Expected to be closer to Color(255,0,0) than Color(0,0,255)
  // closer to 32 than -15
//  Location(-28.0,-176.0): Color(125,0,129).
  val loctemps = Extraction.locateTemperatures(1976,"/stations.csv","/1976.csv")
  val averageloctemps = Extraction.locationYearlyAverageRecords(loctemps)
//  val ref_loc =  Location(-33.0,-158.0)
  val ref_loc = Location(-28, -176)
  println(s"ref_loc = ${ref_loc}")
  val pred_temp = Visualization.predictTemperature(averageloctemps, ref_loc)
  println(s"predicted temperature: ${pred_temp}")
  val interpolated_color = Visualization.interpolateColor(Visualization.getDefaultColours, pred_temp)
  println(s"interpolated colour: ${interpolated_color}")

}

  @Test def `visualisation: check aggdeltasigma`: Unit = {

      val ds_t:Seq[(Double, Temperature)] = Seq(
        (1.0,0.0),
        (2.0,10.0)
      )
      val denom = ds_t.map(x => 1.0/math.pow(x._1,Visualization.inverseDistanceP)).sum
      val actual = Visualization.aggDeltaSigmaTemp(ds_t)
      val expected = ds_t.map(x => x._2/math.pow(x._1,Visualization.inverseDistanceP)).sum/denom
      println("visualisatoion: checkaggdeltasigma")
      println(s"expected: ${expected}, actual: ${actual}")
      assert(math.abs(expected - actual) < TOLERANCE,s"expected: ${expected}, actual: ${actual}")
    }

  @Test def `visualisation: interpolated temps at (-33, -158)`:Unit = {
    val temps = Seq(
      14.423223260610076,
      14.754372975485435,
      15.007796741960252,
      15.075250882823404,
      15.100782554503327,
      15.452644418571058,
      15.183025907500447,
      15.036588153158077,
      14.901673108296695,
      15.041698939120637,
      15.056109635937823,
      15.134656144524095,
      15.073171749347425,
      15.164762798121826,
      15.436182212790504)
    val default_colours = Visualization.getDefaultColours()
    val interp_colours = temps.map(Visualization.interpolateColor(default_colours,_))
    println("interpolated colours for (-33,-158)")
    println("expected closer to (255,0,0) than Color(0,0,255)")
    interp_colours.foreach(println)

//    expected closer to (255,0,0) than Color(0,0,255)

  }

//@Test def `Visualisation: check average temps`:Unit = {
//  val ref_loc = Location(-158, -33.0)
//  val years = (1976 to 1990).toSeq
//  val temps = years.map(x => (x, Visualization.predictTemperature(
//    Extraction.locationYearlyAverageRecords(
//      Extraction.locateTemperatures(x, "/stations.csv", s"/${x}.csv")),
//    ref_loc)))
//  temps.foreach(println)
//}
  // average temps for (-33, -158)
//  (1976,14.423223260610076)
//  (1977,14.754372975485435)
//  (1978,15.007796741960252)
//  (1979,15.075250882823404)
//  (1980,15.100782554503327)
//  (1981,15.452644418571058)
//  (1982,15.183025907500447)
//  (1983,15.036588153158077)
//  (1984,14.901673108296695)
//  (1985,15.041698939120637)
//  (1986,15.056109635937823)
//  (1987,15.134656144524095)
//  (1988,15.073171749347425)
//  (1989,15.164762798121826)
//  (1990,15.436182212790504)
// temps for (-158, -33)
//  (1976,19.84497422987818)
//  (1977,19.865987105124063)
//  (1978,20.53090161581674)
//  (1979,20.640732460595537)
//  (1980,21.42863789251875)
//  (1981,21.04628116079157)
//  (1982,20.785057740868428)
//  (1983,20.527908960758904)
//  (1984,20.26449132632626)
//  (1985,20.501488941450827)
//  (1986,20.68121127664162)
//  (1987,20.98217875541805)
//  (1988,20.82756647589077)
//  (1989,20.108958428938333)
//  (1990,20.313795463911315)
  //
//  computeMeanTemperature - case when distance 0
//  aggDeltaSigmaTemp = quick check

//  val loctemps = Extraction.locateTemperatures(1976,"stations.csv","/1976.csv")
//val averageloctemps = Extraction.locationYearlyAverageRecords(loctemps)


  

}

