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

    assert(3 == r2, "expected 3 but was " + r2)
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

val i4: Img = new Img(3,3,List(
      0,0,0,
      0,28,0,
      0,0,0).toArray)
@Test def `testSimpleBoxBlur2_centre`: Unit = {
    val r01 = boxBlurKernel(i4,1,1,1)
    assert(3 == r01 , "expected 3 but was " + r01)
  }

val i5: Img = new Img(3,3,List(
      0,0,0,
      0,15,0,
      0,0,0).toArray)
@Test def `testSimpleBoxBlur2_corner`: Unit = {
    val r01 = boxBlurKernel(i5,0,0,1)
    assert(3 == r01 , "expected 3 but was " + r01)
  }

// test horizontal sequential blur
@Test def test_horizontalBoxBlur_blur_00: Unit = {
  // allocate memory
  val dst:Img = new Img(3,3,List(
        0,0,0,
        0,0,0,
        0,0,0).toArray)

  HorizontalBoxBlur.blur(i3, dst, 0, 3, 1)
  val result = dst(0,0)
  assert(result == 9 , s"expected 9 but was $result")
}

@Test def test_horizontalBoxBlur_blur_11: Unit = {
  // allocate memory
  val dst:Img = new Img(3,3,List(
        0,0,0,
        0,0,0,
        0,0,0).toArray)

  HorizontalBoxBlur.blur(i3, dst, 0, 3, 1)
  val result = dst(1,1)
  assert(result == 4 , s"expected 4 but was $result")
}

@Test def test_horizontalBoxBlur_blur_12: Unit = {
  // allocate memory
  val dst:Img = new Img(3,3,List(
        0,0,0,
        0,0,0,
        0,0,0).toArray)

  HorizontalBoxBlur.blur(i3, dst, 0, 3, 1)
  val result = dst(1,2)
  assert(result == 6 , s"expected 6 but was $result")
}

@Test def test_horizontalBoxBlur_blur_01: Unit = {
  // allocate memory
  val dst:Img = new Img(3,3,List(
        0,0,0,
        0,0,0,
        0,0,0).toArray)

  HorizontalBoxBlur.blur(i3, dst, 0, 3, 1)
  val result = dst(0,1)
  assert(result == 6 , s"expected 6 but was $result")
}

@Test def test_horizontalBoxBlur_parblur_1task_00: Unit = {
  // allocate memory
  val dst:Img = new Img(3,3,List(
        0,0,0,
        0,0,0,
        0,0,0).toArray)

  HorizontalBoxBlur.parBlur(i3, dst, 1, 1)
  val result = dst(0,0)
  assert(result == 9 , s"expected 9 but was $result")
}

@Test def test_horizontalBoxBlur_parblur_1task_11: Unit = {
  // allocate memory
  val dst:Img = new Img(3,3,List(
        0,0,0,
        0,0,0,
        0,0,0).toArray)

  HorizontalBoxBlur.parBlur(i3, dst, 1, 1)
  val result = dst(1,1)
  assert(result == 4 , s"expected 4 but was $result")
}

@Test def test_horizontalBoxBlur_parblur_1000task_00: Unit = {
  // allocate memory
  val dst:Img = new Img(3,3,List(
        0,0,0,
        0,0,0,
        0,0,0).toArray)

  HorizontalBoxBlur.parBlur(i3, dst, 1000, 1)
  val result = dst(0,0)
  assert(result == 9 , s"expected 9 but was $result")
}

@Test def test_horizontalBoxBlur_parblur_1000task_11: Unit = {
  // allocate memory
  val dst:Img = new Img(3,3,List(
        0,0,0,
        0,0,0,
        0,0,0).toArray)

  HorizontalBoxBlur.parBlur(i3, dst, 1000, 1)
  val result = dst(1,1)
  assert(result == 4 , s"expected 4 but was $result")
}


// test vertical sequential blur
@Test def test_verticalBoxBlur_blur_00: Unit = {
  // allocate memory
  val dst:Img = new Img(3,3,List(
        0,0,0,
        0,0,0,
        0,0,0).toArray)

  VerticalBoxBlur.blur(i3, dst, 0, 3, 1)
  val result = dst(0,0)
  assert(result == 9 , s"expected 9 but was $result")
}

@Test def test_verticalBoxBlur_blur_11: Unit = {
  // allocate memory
  val dst:Img = new Img(3,3,List(
        0,0,0,
        0,0,0,
        0,0,0).toArray)

  VerticalBoxBlur.blur(i3, dst, 0, 3, 1)
  val result = dst(1,1)
  assert(result == 4 , s"expected 4 but was $result")
}

@Test def test_verticalBoxBlur_blur_12: Unit = {
  // allocate memory
  val dst:Img = new Img(3,3,List(
        0,0,0,
        0,0,0,
        0,0,0).toArray)

  VerticalBoxBlur.blur(i3, dst, 0, 3, 1)
  val result = dst(1,2)
  assert(result == 6 , s"expected 6 but was $result")
}

@Test def test_verticalBoxBlur_blur_01: Unit = {
  // allocate memory
  val dst:Img = new Img(3,3,List(
        0,0,0,
        0,0,0,
        0,0,0).toArray)

  VerticalBoxBlur.blur(i3, dst, 0, 3, 1)
  val result = dst(0,1)
  assert(result == 6 , s"expected 6 but was $result")
}

@Test def test_verticalBoxBlur_parblur_1task_00: Unit = {
  // allocate memory
  val dst:Img = new Img(3,3,List(
        0,0,0,
        0,0,0,
        0,0,0).toArray)

  VerticalBoxBlur.parBlur(i3, dst, 1, 1)
  val result = dst(0,0)
  assert(result == 9 , s"expected 9 but was $result")
}

@Test def test_verticalBoxBlur_parblur_1task_11: Unit = {
  // allocate memory
  val dst:Img = new Img(3,3,List(
        0,0,0,
        0,0,0,
        0,0,0).toArray)

  VerticalBoxBlur.parBlur(i3, dst, 1, 1)
  val result = dst(1,1)
  assert(result == 4 , s"expected 4 but was $result")
}

@Test def test_verticalBoxBlur_parblur_1000task_00: Unit = {
  // allocate memory
  val dst:Img = new Img(3,3,List(
        0,0,0,
        0,0,0,
        0,0,0).toArray)

  VerticalBoxBlur.parBlur(i3, dst, 1000, 1)
  val result = dst(0,0)
  assert(result == 9 , s"expected 9 but was $result")
}

@Test def test_verticalBoxBlur_parblur_1000task_11: Unit = {
  // allocate memory
  val dst:Img = new Img(3,3,List(
        0,0,0,
        0,0,0,
        0,0,0).toArray)

  VerticalBoxBlur.parBlur(i3, dst, 1000, 1)
  val result = dst(1,1)
  assert(result == 4 , s"expected 4 but was $result")
}


@Test def test_verticalBoxBlur_parblur_1task_entire: Unit = {
  // allocate memory
val img_seq:Img = new Img(3,3,List(
        1,2,3,
        4,5,6,
        7,8,9).toArray)

  val dst:Img = new Img(3,3,List(
        0,0,0,
        0,0,0,
        0,0,0).toArray)

  VerticalBoxBlur.parBlur(img_seq, dst, 1000, 1)

  var result = dst(0,0)
  var expected = (1+2+4+5)/4
  var pixel = (0,0)
  assert(result == expected , s"pixel $pixel expected $expected but was $result")

  result = dst(1,0)
  expected = (1+2+3+4+5+6)/6
  pixel = (1,0)
  assert(result == expected , s"pixel $pixel expected $expected but was $result")

  result = dst(2,0)
  expected = (3+4+5+6)/4
  pixel = (2,0)
  assert(result == expected , s"pixel $pixel expected $expected but was $result")

  result = dst(0,1)
  expected = (1+2+4+5+7+8)/6
  pixel = (0,1)
  assert(result == expected , s"pixel $pixel expected $expected but was $result")

  result = dst(1,1)
  expected = (1+2+3+4+5+6+7+8+9)/9
  pixel = (1,1)
  assert(result == expected , s"pixel $pixel expected $expected but was $result")

  result = dst(2,1)
  expected = (2+3+5+6+8+9)/6
  pixel = (2,1)
  assert(result == expected , s"pixel $pixel expected $expected but was $result")

  result = dst(0,2)
  expected = (4+5+7+8)/4
  pixel = (0,2)
  assert(result == expected , s"pixel $pixel expected $expected but was $result")

  result = dst(1,2)
  expected = (4+5+6+7+8+9)/6
  pixel = (1,2)
  assert(result == expected , s"pixel $pixel expected $expected but was $result")

  result = dst(2,2)
  expected = (5+6+8+9)/4
  pixel = (2,2)
  assert(result == expected , s"pixel $pixel expected $expected but was $result")

}

@Test def test_HorizontalBoxBlur_parblur_1task_entire: Unit = {
  // allocate memory
val img_seq:Img = new Img(3,3,List(
        1,2,3,
        4,5,6,
        7,8,9).toArray)

  val dst:Img = new Img(3,3,List(
        0,0,0,
        0,0,0,
        0,0,0).toArray)

  HorizontalBoxBlur.parBlur(img_seq, dst, 1000, 1)

  var result = dst(0,0)
  var expected = (1+2+4+5)/4
  var pixel = (0,0)
  assert(result == expected , s"pixel $pixel expected $expected but was $result")

  result = dst(1,0)
  expected = (1+2+3+4+5+6)/6
  pixel = (1,0)
  assert(result == expected , s"pixel $pixel expected $expected but was $result")

  result = dst(2,0)
  expected = (3+4+5+6)/4
  pixel = (2,0)
  assert(result == expected , s"pixel $pixel expected $expected but was $result")

  result = dst(0,1)
  expected = (1+2+4+5+7+8)/6
  pixel = (0,1)
  assert(result == expected , s"pixel $pixel expected $expected but was $result")

  result = dst(1,1)
  expected = (1+2+3+4+5+6+7+8+9)/9
  pixel = (1,1)
  assert(result == expected , s"pixel $pixel expected $expected but was $result")

  result = dst(2,1)
  expected = (2+3+5+6+8+9)/6
  pixel = (2,1)
  assert(result == expected , s"pixel $pixel expected $expected but was $result")

  result = dst(0,2)
  expected = (4+5+7+8)/4
  pixel = (0,2)
  assert(result == expected , s"pixel $pixel expected $expected but was $result")

  result = dst(1,2)
  expected = (4+5+6+7+8+9)/6
  pixel = (1,2)
  assert(result == expected , s"pixel $pixel expected $expected but was $result")

  result = dst(2,2)
  expected = (5+6+8+9)/4
  pixel = (2,2)
  assert(result == expected , s"pixel $pixel expected $expected but was $result")

}
}

