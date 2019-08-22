package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s123 = List(1,2,3).map(singletonSet).reduce(union)
    val emptySet = (x:Int) => false
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }
  // PT
  // intersect, diff, filter, forall, exists, map
  test("intersect contains an intersection") {
    new TestSets {
      val sIntersection = intersect(s1, s123)
      assert(contains(sIntersection, 1), "intersection 1")
      assert(!contains(sIntersection, 2), "intersection 2")
      assert(!contains(sIntersection, 3), "intersection 3")
    }
  }
  test("intersect can yield an empty set") {
    new TestSets {
      val emptyIntersection = intersect(s1, s3)
      assert(!contains(emptyIntersection, 1), "empty intersection 1")
      assert(!contains(emptyIntersection, 2), "empty intersection 2")
      assert(!contains(emptyIntersection, 3), "empty intersection 3")
    }
  }


  test("diff yields a difference") {
    new TestSets {
      val theDiff = diff(s123, s2)
      assert(contains(theDiff, 1), "difference 1")
      assert(!contains(theDiff, 2), "difference 2")
      assert(contains(theDiff, 3), "difference 3")
    }
  }

  test("filter picks out odd numbers") {
    new TestSets {
      val sFiltered = filter(s123, (x:Int) => x%2 != 0)
      assert(contains(sFiltered, 1), "filter 1")
      assert(!contains(sFiltered, 2), "filter 2")
      assert(contains(sFiltered, 3), "filter 3")
    }
  }

  test("forall works") {
    new TestSets {
      assert(!forall(s123, (x:Int) => x%2 == 0), "forall 1")
      assert(forall(s2, (x:Int) => x%2 == 0), "forall 2")
      assert(forall(emptySet, (x:Int) => true),"forall empty")
    }
  }
  test("exists works") {
    new TestSets {
      assert(exists(s123, (x:Int) => x%2 == 0), "exists 1")
      assert(!exists(s2, (x:Int) => x>6), "exists 2")
      assert(!exists(emptySet, (x:Int) => true),"exists empty")
    }
  }
  // Map
  test("Map works 1") {
    new TestSets {
      val mapped = map(s123,(x:Int) =>x*2)
      assert(forall(mapped, (x:Int) => x%2 == 0), "divisible by 2")
      assert(contains(mapped,2),"contains 2")
      assert(contains(mapped,4),"contains 2")
      assert(contains(mapped,6),"contains 2")
    }
  }


}
