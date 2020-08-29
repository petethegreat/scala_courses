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






//  computeMeanTemperature - case when distance 0
//  aggDeltaSigmaTemp = quick check


}
