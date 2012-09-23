package recfun

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PascalSuite extends FunSuite {
  import Main.pascal
  
  test("col=0,row=2") {
    assert(pascal(0,2) === 1)
  }

  test("col=1,row=2") {
    assert(pascal(1,2) === 2)
  }

  test("col=1,row=3") {
    assert(pascal(1,3) === 3)
  }

  test("col=0,row=0") {
    assert(pascal(0,0) === 1)
  }

  test("col=0,row=1") {
    assert(pascal(0,1) === 1)
  }  
  
  test("col=2,row=2") {
    assert(pascal(2,2) === 1)
  }

  test("col=1,row=10") {
    assert(pascal(1,10) === 10)
  }    
  
  test("col=2,row=10") {
    assert(pascal(2,10) === 45)
  }    
  
  test("col=5,row=10") {
    assert(pascal(5,10) === 252)
  }  
  
  test("col=-1,row=0") {
    intercept[IllegalArgumentException] {
    	pascal(-1,0)
    }
  }
  
  test("col=0,row=-1") {
    intercept[IllegalArgumentException] {
    	pascal(0,-1)
    }
  }    
  
  test("col=1,row=0") {
    intercept[IllegalArgumentException] {
    	pascal(1,0)
    }
  }  
}
