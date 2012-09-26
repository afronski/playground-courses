package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  trait TestSets {
    val firstSet = singletonSet(1)
    val secondSet = singletonSet(2)
    val thirdSet = singletonSet(3)
  }

  test("singletonSets tests") {

    new TestSets {
      assert(contains(firstSet, 1), "Singleton s1")
      assert(contains(secondSet, 2), "Singleton s2")
      assert(contains(thirdSet, 3), "Singleton s3")

      assert(!contains(firstSet, 2), "Singleton s1 => false case")
      assert(!contains(secondSet, 3), "Singleton s2 => false case")
      assert(!contains(thirdSet, 1), "Singleton s3 => false case")
    }
  }

  test("union contains all elements") {
    new TestSets {
      val result = union(firstSet, secondSet)

      assert(contains(result, 1), "Union 1")
      assert(contains(result, 2), "Union 2")
      assert(!contains(result, 3), "Union 3")
    }
  }

  test("union of custom sets") {
    val firstSet = (x: Int) => x > 10
    val secondSet = (x: Int) => x < 20
    val result = union(firstSet, secondSet)

    assert(contains(result, 1))
    assert(contains(result, 10))
    assert(contains(result, 20))
    assert(contains(result, 100))
    assert(contains(result, 9))
    assert(contains(result, 19))
    assert(contains(result, 15))
  }

  test("insersect two excluding sets") {
    new TestSets {
      val result = intersect(firstSet, secondSet)

      assert(!contains(result, 1), "Intersect 1")
      assert(!contains(result, 2), "Intersect 2")
      assert(!contains(result, 3), "Intersect 3")
    }
  }

  test("intersect of custom sets") {
    val firstSet = (x: Int) => x > 10
    val secondSet = (x: Int) => x < 20
    val result = intersect(firstSet, secondSet)

    assert(!contains(result, 1), "Set cannot contains 1")
    assert(!contains(result, 10), "Set cannot contains 10")
    assert(!contains(result, 20), "Set cannot contains 20")
    assert(!contains(result, 100), "Set cannot contains 100")

    assert(contains(result, 11), "Set must contains 11")
    assert(contains(result, 19), "Set must contains 19")
    assert(contains(result, 15), "Set must contains 15")
  }

  test("diff of custom sets") {
    val firstSet = (x: Int) => x > 10
    val secondSet = (x: Int) => x > 20
    val result = diff(firstSet, secondSet)

    assert(!contains(result, 1), "Set mustn't contains 1")
    assert(!contains(result, 10), "Set must'nt contains 10")
    assert(contains(result, 11), "Set must contains 11")
    assert(contains(result, 19), "Set must contains 19")
    assert(contains(result, 15), "Set must contains 15")
    assert(!contains(result, 21), "Set mustn't contains 21")
    assert(!contains(result, 100), "Set mustn't contains 100")
  }

  test("filter is an intersection") {
    val set = (x: Int) => x > 10
    val result = filter(set, (x: Int) => x < 20)

    assert(!contains(result, 1), "Set cannot contains 1")
    assert(!contains(result, 10), "Set cannot contains 10")
    assert(!contains(result, 20), "Set cannot contains 20")
    assert(!contains(result, 100), "Set cannot contains 100")

    assert(contains(result, 11), "Set must contains 11")
    assert(contains(result, 19), "Set must contains 19")
    assert(contains(result, 15), "Set must contains 15")
  }

  test("forall test") {
    val set = (x: Int) => x > 10

    val predicate1 = (x: Int) => x > 10
    val predicate2 = (x: Int) => x > 11
    val predicate3 = (x: Int) => x < -9
    val predicate4 = (x: Int) => x != 0

    val predicate5 = (x: Int) => x > 10000
    val predicate6 = (x: Int) => x < -10000

    assert(forall(set, predicate1), "Test 1")
    assert(!forall(set, predicate2), "Test 2")
    assert(!forall(set, predicate3), "Test 3")
    assert(forall(set, predicate4), "Test 4")

    assert(!forall(set, predicate5), "Test 5")
    assert(!forall(set, predicate6), "Test 6")
  }

  test("exists test") {
    val set = (x: Int) => x > 10

    val predicate1 = (x: Int) => x == 10
    val predicate2 = (x: Int) => x == 11
    val predicate3 = (x: Int) => x == -9
    val predicate4 = (x: Int) => x == 100

    val predicate5 = (x: Int) => x == 10000
    val predicate6 = (x: Int) => x == -10000

    assert(!exists(set, predicate1), "Test 1")
    assert(exists(set, predicate2), "Test 2")
    assert(!exists(set, predicate3), "Test 3")
    assert(exists(set, predicate4), "Test 4")

    assert(!exists(set, predicate5), "Test 5")
    assert(!exists(set, predicate6), "Test 6")
  }

  test("adding one modificator - map test") {
    val sourceSet = (x : Int) => x == 0 || x == 2 || x == 4 || x == 1000
    val modificator = (x : Int) => x + 1
        
    assert(!exists(map(sourceSet, modificator), p => p == 0), "Not possible = 0")
    assert(exists(map(sourceSet, modificator), p => p == 1), "Possible = 1")
    assert(!exists(map(sourceSet, modificator), p => p == 2), "Not possible = 2")
    assert(exists(map(sourceSet, modificator), p => p == 3), "Possible = 3")
    assert(!exists(map(sourceSet,modificator), p => p == 4), "Not possible = 4")
    assert(exists(map(sourceSet, modificator), p => p == 5), "Possible = 5")
    assert(!exists(map(sourceSet, modificator), p => p == 1000), "Not possible = 1000")
    assert(!exists(map(sourceSet, modificator), p => p == 1001), "Not possible = 1001")        
  }
  
  test("subtracting one modificator - map test") {
    val sourceSet = (x : Int) => x == 0 || x == 2 || x == 4 || x == 1000
    val modificator = (x : Int) => x - 1
        
    assert(!exists(map(sourceSet, modificator), p => p == 0), "Not possible = 0")
    assert(exists(map(sourceSet, modificator), p => p == -1), "Possible = 1")
    assert(!exists(map(sourceSet, modificator), p => p == 2), "Not possible = 2")
    assert(exists(map(sourceSet, modificator), p => p == 1), "Possible = 3")
    assert(!exists(map(sourceSet,modificator), p => p == 4), "Not possible = 4")
    assert(exists(map(sourceSet, modificator), p => p == 3), "Possible = 3")
    assert(!exists(map(sourceSet, modificator), p => p == 1000), "Not possible = 1000")
    assert(exists(map(sourceSet, modificator), p => p == 999), "Possible = 999")        
  }  
  
  test("multiply by 2 - map test") {
    val sourceSet = (x : Int) => x == 1 || x == 3 || x == 5 || x == 7
    val modificator = (x : Int) => x * 2
        
    assert(forall(sourceSet, p => p % 2 != 0), "Odd on start")
    assert(exists(map(sourceSet, modificator), p => p == 2), "Even after map = 2")
    assert(forall(map(sourceSet, modificator), p => p % 2 == 0), "Even after map = all")   
  }    
}
