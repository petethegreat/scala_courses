package scalashop

import java.util.concurrent._
import scala.collection._
import org.junit._
import org.junit.Assert.assertEquals

class BlurSuite {

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)

  @Test def `testSimpleBoxBlur_1`: Unit = {

    val i: Img = new Img(3, 3, List(2,2,2,2,1,2,2,2,2).toArray)

    val r = boxBlurKernel(i,1,1,1)

    val e: RGBA = 1

    assert(e == r, "expected 1 but was " + r)
  }
  @Test def `testSimpleBoxBlur_1a`: Unit = {

    val i: Img = new Img(3, 3, List(2,2,2,2,2,2,2,2,2).toArray)

    val r = boxBlurKernel(i,1,1,1)

    val e: RGBA = 2

    assert(e == r, "expected 2 but was " + r)
  }

  @Test def `testSimpleBoxBlur_2`: Unit = {

    val i2: Img = new Img(3, 3, List(2,2,2,2,3,4,4,4,4).toArray)

    val r2 = boxBlurKernel(i2,1,1,1)

    assert(2 == r2, "expected 3 but was " + r2)
  }

val i3: Img = new Img(3,3,List(
      0,0,0,
      0,36,0,
      0,0,0).toArray)

@Test def `testSimpleBoxBlur_windowCorner`: Unit = {
    
    val r00 = boxBlurKernel(i3,0,0,1)
    assert(9 == r00, "expected 9 but was " + r00)
  }
@Test def `testSimpleBoxBlur_windowCentre`: Unit = {
    val r11 = boxBlurKernel(i3,1,1,1)
    assert(4 == r11, "expected 4 but was " + r11)
  }
@Test def `testSimpleBoxBlur_windowEdge`: Unit = {
    val r01 = boxBlurKernel(i3,0,1,1)
    assert(6 == r01 , "expected 6 but was " + r01)
  }







  }



