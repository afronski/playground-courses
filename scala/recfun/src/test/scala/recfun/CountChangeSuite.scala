package recfun

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CountChangeSuite extends FunSuite {
  import Main.countChange
  
  test("no coins at all") {
    assert(countChange(4,List()) === 0)
  }

  test("how many times we change a negative amount of money") {
    assert(countChange(-1,List(1)) === 0)
  }
  
  test("how many times we change a zero amount of money") {
    assert(countChange(0,List(1)) === 1)
  }
  
  test("example from instructions") {
    assert(countChange(4,List(1,2)) === 3)
  }
  
  test("changes are not commutative") {
    assert(countChange(3,List(1,2)) === 2)
  }  
  
  test("another sample") {
    assert(countChange(100,List(100, 50, 25, 10, 5, 1)) == 293)
  }
  
  test("sorted CHF") {
    assert(countChange(300,List(5,10,20,50,100,200,500)) === 1022)
  }

  test("no pennies") {
    assert(countChange(301,List(5,10,20,50,100,200,500)) === 0)
  }

  test("unsorted CHF") {
    assert(countChange(300,List(500,5,50,100,20,200,10)) === 1022)
  }
}
