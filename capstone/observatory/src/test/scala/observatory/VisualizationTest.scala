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

  @Test def `visualisation: correct lat/lon values`: Unit = {

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
      Color(16,0,53),
      Color(33,0,107),
      Color(255,0,255),
      Color(255,127,0),
      Color(255,255,255),
      Color(255,255,255))
    val actual = inputTemps.map(Visualization.interpolateColor(colours,_))
    assert(expected == actual, "interpolated colours differ from expected")


    println()
    println("colour interpolation results")
    results.foreach(println)
  }




//  computeMeanTemperature - case when distance 0
//  aggDeltaSigmaTemp = quick check


}
