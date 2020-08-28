package observatory

import org.junit.Assert._
import org.junit.Test

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
    val l1 = Location(87.0,36.5)
    val l2 = Location(-87.0,216.5)
    val expected = math.Pi
    val actual = Visualization.getLocationDifference(l1,l2)
    assert(expected == actual, s"expected ${expected}, actual ${actual}")
  }

  @Test def `visualisation: check deltasigma other` = {
    val l1 = Location(90.0,36.5)
    val l2 = Location(80.0,216.5)
    val expected = 0.17453292519943317
    val actual = Visualization.getLocationDifference(l1,l2)
    assert(math.abs(expected - actual) < TOLERANCE, s"expected ${expected}, actual ${actual}")
  }

}
