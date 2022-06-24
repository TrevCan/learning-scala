package funsets

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite extends munit.FunSuite:

  import FunSets.*

  test("contains is implemented") {
    assert(contains(x => true, 100))
    assert(contains(x => x==1, 1))
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

  trait TestSets:
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)

    def allpositives: FunSet =
      (x: Int) => x > 0

    def allnegatives: FunSet = 
      (x: Int) => x < 0



  /**
   * This test is currently disabled (by using @Ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", remove the
   * .ignore annotation.
   */
  test("singleton set one contains one") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets:
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton 1 failed")
      assert(contains(s2, 2), "Singleton 2 failed")
      assert(!contains(s3, 2), "Singleton 3 failed")
  }

  test("union contains all elements of each set") {
    new TestSets:
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
  }

  test("forall computes for itself"){
    new TestSets:
      assert(forall(s1, s1), "singleton 1")
      assert(forall(s2, s2), "singleton 2")
      assert(forall(s3, s3), "singleton 3")


      assert(forall(allpositives, allpositives), "positives contain all positives")
      assert(forall(allnegatives, allnegatives), "negatives contain all negatives")
      assert(!forall(allnegatives, allpositives), "NEgatives do not contain positives")
      assert(!forall(allpositives, allnegatives), "POsitives do not contain negatives")
    
  }

  test("constant checking positives and negatives"){

    new TestSets:
      assert(!contains(allpositives, -99), "POsitives does not contain -99")
      assert(!contains(allnegatives,  99), "NEgatives does not contain  99")

      assert(contains(allpositives,  1000), "POsitives does contain     1000")
      assert(contains(allnegatives, -1000), "NEgatives does contain    -1000")

  }



  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds
