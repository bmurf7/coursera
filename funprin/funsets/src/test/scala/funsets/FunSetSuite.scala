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
      val u = union(s1, s2)
      assert(contains(u, 1), "Union 1")
      assert(contains(u, 2), "Union 2")
      assert(!contains(u, 3), "Union 3")
    }
  }

  test("intersect") {
    new TestSets {
      val u = union(s1, s2)
      val t = intersect(u, s2)
      assert(!contains(t, 1), "Intersect 1")
      assert(contains(t, 2), "Intersect 2")
    }
  }
  test("diff") {
    new TestSets {
      val u = union(s1, s2)
      val d = diff(u, s2)
      assert(contains(d, 1), "Diff 1")
      assert(!contains(d, 2), "Diff 2")
    }
  }
  test("filter") {
    new TestSets {
      assert(!contains(filter(singletonSet(1), (x: Int) => x%2==0), 1), "filter 1")
      assert(contains(filter(singletonSet(2), (x: Int) => x%2==0), 2), "filter 2")
      assert(!contains(filter(singletonSet(13), (x: Int) => x%2==0), 13), "filter 3")
      assert(contains(filter(singletonSet(22), (x: Int) => x%2==0), 22), "filter 4")
      //filter of {1,3,4,5,7,1000} for _ < 5
      assert(contains(filter(singletonSet(-1000), (x: Int) => x < 5), -1000), "filter 5")
      assert(contains(filter(singletonSet(-7), (x: Int) => x < 5), -7), "filter 5")
      assert(contains(filter(singletonSet(1), (x: Int) => x < 5), 1), "filter 5")
      assert(contains(filter(singletonSet(3), (x: Int) => x < 5), 3), "filter 5")
      assert(!contains(filter(singletonSet(5), (x: Int) => x < 5), 5), "filter 5")
      assert(!contains(filter(singletonSet(7), (x: Int) => x < 5), 7), "filter 5")
      assert(!contains(filter(singletonSet(1000), (x: Int) => x < 5), 1000), "filter 5")
    }
  }
  test("forall") {
    assert(forall((x: Int) => x % 2 == 0, (x: Int) => x % 2 == 0), "even")
    assert(!forall((x: Int) => x % 2 == 0, (x: Int) => x % 2 == 1), "even")
  }
  test("exists") {
    assert(exists((x: Int) => true, (x: Int) => x==13))
    assert(!exists((x: Int) => x > 100, (x: Int) => x==13))
    assert(!exists((x: Int) => x%2==0 , (x: Int) => x%2==1))
    assert(exists((x: Int) => (x >= 40 && x < 50), (x: Int) => x%2==1))
  }
  test("map: {1,3,4,5,7,1000}") {
    //"{[2,4,5,6,8]}" did not equal "{[0,2,3,4,6,999]}"
    val sub1 = (x: Int) => x - 1
    assert(contains(map((x: Int) => List(2,4,5,6,8).contains(x),sub1), 1), "map 1")
    assert(!contains(map((x: Int) => List(2,4,5,6,8).contains(x),sub1), 2), "map 2")
    assert(contains(map((x: Int) => List(2,4,5,6,8).contains(x),sub1), 3), "map 3")
  }
}
