package barneshut

import java.util.concurrent._
import scala.collection._
import scala.math._
import scala.collection.parallel._
import barneshut.conctrees.ConcBuffer
import org.junit._
import org.junit.Assert.{assertEquals, fail}

class BarnesHutSuite {
  // test cases for quad tree

import FloatOps._
  @Test def `Empty: center of mass should be the center of the cell`: Unit = {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.massX == 51f, s"${quad.massX} should be 51f")
    assert(quad.massY == 46.3f, s"${quad.massY} should be 46.3f")
  }

  @Test def `Empty: mass should be 0`: Unit = {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.mass == 0f, s"${quad.mass} should be 0f")
  }

  @Test def `Empty: total should be 0`: Unit = {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.total == 0, s"${quad.total} should be 0")
  }

  @Test def `Leaf with 1 body`: Unit = {
    val b = new Body(123f, 18f, 26f, 0f, 0f)
    val quad = Leaf(17.5f, 27.5f, 5f, Seq(b))

    assert(quad.mass ~= 123f, s"${quad.mass} should be 123f")
    assert(quad.massX ~= 18f, s"${quad.massX} should be 18f")
    assert(quad.massY ~= 26f, s"${quad.massY} should be 26f")
    assert(quad.total == 1, s"${quad.total} should be 1")
  }


  @Test def `Fork with 3 empty quadrants and 1 leaf (nw)`: Unit = {
    val b = new Body(123f, 18f, 26f, 0f, 0f)
    val nw = Leaf(17.5f, 27.5f, 5f, Seq(b))
    val ne = Empty(22.5f, 27.5f, 5f)
    val sw = Empty(17.5f, 32.5f, 5f)
    val se = Empty(22.5f, 32.5f, 5f)
    val quad = Fork(nw, ne, sw, se)

    assert(quad.centerX == 20f, s"${quad.centerX} should be 20f")
    assert(quad.centerY == 30f, s"${quad.centerY} should be 30f")
    assert(quad.mass ~= 123f, s"${quad.mass} should be 123f")
    assert(quad.massX ~= 18f, s"${quad.massX} should be 18f")
    assert(quad.massY ~= 26f, s"${quad.massY} should be 26f")
    assert(quad.total == 1, s"${quad.total} should be 1")
  }

  @Test def `Empty.insert(b) should return a Leaf with only that body (2pts)`: Unit = {
    val quad = Empty(51f, 46.3f, 5f)
    val b = new Body(3f, 54f, 46f, 0f, 0f)
    val inserted = quad.insert(b)
    inserted match {
      case Leaf(centerX, centerY, size, bodies) =>
        assert(centerX == 51f, s"$centerX should be 51f")
        assert(centerY == 46.3f, s"$centerY should be 46.3f")
        assert(size == 5f, s"$size should be 5f")
        assert(bodies == Seq(b), s"$bodies should contain only the inserted body")
      case _ =>
        fail("Empty.insert() should have returned a Leaf, was $inserted")
    }
  }

  // test cases for Body

  @Test def `Body.updated should do nothing for Empty quad trees`: Unit = {
    val b1 = new Body(123f, 18f, 26f, 0f, 0f)
    val body = b1.updated(Empty(50f, 60f, 5f))

    assertEquals(0f, body.xspeed, precisionThreshold)
    assertEquals(0f, body.yspeed, precisionThreshold)
  }

  @Test def `Body.updated should take bodies in a Leaf into account (2pts)`: Unit = {
    val b1 = new Body(123f, 18f, 26f, 0f, 0f)
    val b2 = new Body(524.5f, 24.5f, 25.5f, 0f, 0f)
    val b3 = new Body(245f, 22.4f, 41f, 0f, 0f)

    val quad = Leaf(15f, 30f, 20f, Seq(b2, b3))

    val body = b1.updated(quad)

    assert(body.xspeed ~= 12.587037f)
    assert(body.yspeed ~= 0.015557117f)
  }

  // test cases for sector matrix

  @Test def `'SectorMatrix.+=' should add a body at (25,47) to the correct bucket of a sector matrix of size 96 (2pts)`: Unit = {
    val body = new Body(5, 25, 47, 0.1f, 0.1f)
    val boundaries = new Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 97
    boundaries.maxY = 97
    val sm = new SectorMatrix(boundaries, SECTOR_PRECISION)
    sm += body
    val res = sm(2, 3).size == 1 && sm(2, 3).find(_ == body).isDefined
    assert(res, s"Body not found in the right sector")
  }

  // pete tests
  @Test def `mergeboundaries should produce a rectangle from (-1,1) to (4,6) when two rectangles are merged`: Unit = {
    val a = new Boundaries()
    val b = new Boundaries()
    a.minX = -1
    a.maxX = 2
    a.minY = 1
    a.maxY = 3
    b.minX = 5
    b.maxX = 6
    b.minY = 4
    b.maxY = 6
    val simulator = new Simulator(defaultTaskSupport,new TimeStatistics)
    val merged = simulator.mergeBoundaries(a,b)
    val res = (merged.minX,merged.minY,merged.maxX,merged.maxY)
    val expected = (a.minX,a.minY,b.maxX,b.maxY)

    assert( res == expected, s" expected $expected, got $res")
  }

  @Test def `updateBoundaries should produce a rectangle from (-1,1) to (4,6) when a body is added`: Unit = {
    val a = new Boundaries()
    a.minX = -1
    a.maxX = 2
    a.minY = 1
    a.maxY = 3
    var b = new Body(2.2f,4f,6f,1,1)
    val simulator = new Simulator(defaultTaskSupport,new TimeStatistics)
    val updated = simulator.updateBoundaries(a,b)
    val res = (updated.minX,updated.minY,updated.maxX,updated.maxY)
    val expected = (a.minX,a.minY,4f,6f)
    assert( res == expected, s" expected $expected, got $res")
  }
@Test def `check quad insertion`: Unit = {
  //var quad = Empty(0,0,20)
  val xpos =Seq(-4f,5f,2f,3f,4f,6f,0f,4f)
  val bodies = xpos.map( x => new Body(1f,x,0f,0f,0f))
  var quad = bodies.foldLeft(new Empty(0,0,20): Quad) ((q,b) => q.insert(b))
  println(s"inserted = ${quad.total} bodies")

  //for (b <- bodies) {quad = quad.insert(b)}

  val total = quad.total
  val expected = xpos.length
  assert(expected == total, s" expected bodies: $expected, actual total: $total")
}

@Test def `computeSectorMatrix should work correctly`: Unit = {
  // boundaries
  val b = new Boundaries
  b.minX = -10f
  b.maxX = 10f
  b.minY = -10f
  b.maxY = 10f

  // add some bodies
  val xpos =Seq(-4f,5f,2f,3f,4f,6f,0f,4f)
  val bodies = xpos.map( x => new Body(1f,x,x,0f,0f))

  val simulator = new Simulator(defaultTaskSupport,new TimeStatistics)
  val sm = simulator.computeSectorMatrix(bodies,b)
  println(sm)
  val quad = sm.toQuad(4)

  val expected = xpos.length
  val result = quad.total
  println(quad)

  assert(expected == result, s"expected $expected bodies in quadtree, total of $result bodies instead")

}

//  @Test def `quadtree_recursive insertion`: Unit = {
//    val thesize = 10
//    val body = new Body(100f,0f,0f,0f,0f)
//    var quad:Quad = new Empty(5,5,thesize)
//    val nbodies = 4
//    for (i <- 0 to nbodies) quad = quad.insert(body)
////    i = 0 -> leaf
////    i = 1 -> fork (size 5)
//    val expected = thesize/nbodies.toFloat
//    println(quad)
//    println(quad.total)
//    println(s"expected size $expected, got ${quad.centerX}")
//    assert (quad.centerX == expected,s"expected size $expected, got ${quad.centerX}")
//  }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}

object FloatOps {
  val precisionThreshold = 1e-4

  /** Floating comparison: assert(float ~= 1.7f). */
  implicit class FloatOps(val self: Float) extends AnyVal {
    def ~=(that: Float): Boolean =
      abs(self - that) < precisionThreshold
  }

  /** Long floating comparison: assert(double ~= 1.7). */
  implicit class DoubleOps(val self: Double) extends AnyVal {
    def ~=(that: Double): Boolean =
      abs(self - that) < precisionThreshold
  }

  /** Floating sequences comparison: assert(floatSeq ~= Seq(0.5f, 1.7f). */
  implicit class FloatSequenceOps(val self: Seq[Float]) extends AnyVal {
    def ~=(that: Seq[Float]): Boolean =
      self.size == that.size &&
        self.zip(that).forall { case (a, b) =>
          abs(a - b) < precisionThreshold
        }
  }
}

