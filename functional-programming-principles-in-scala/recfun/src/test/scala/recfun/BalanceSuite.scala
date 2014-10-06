package recfun

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BalanceSuite extends FunSuite {
  import Main.balance

  test("'(if (zero? x) max (/ 1 x))' is balanced.") {
    assert(balance("(if (zero? x) max (/ 1 x))".toList))
  }

  test("'I told him ...' is balanced.") {
    assert(balance("I told him (that it's not (yet) done).\n(But he wasn't listening)".toList))
  }

  test("':-)' is unbalanced.") {
    assert(!balance(":-)".toList))
  }

  test("'unmatched parenthesis leave weird tension all day long)' is unbalanced.") {
    assert(!balance("unmatched parenthesis leave weird tension all day long)".toList))
  }  
    
  test("Counting is not enough.") {
    assert(!balance("())(".toList))
  }
    
  test("Counting is not enough - part 2.") {
    assert(!balance("()(".toList))
  }  
  
  test("Counting is not enough - part 3.") {
    assert(!balance("()((".toList))
  }    
  
  test("Counting is not enough - part 4.") {
    assert(balance("()()".toList))
  }      
  
  test("Counting is not enough - part 5.") {
    assert(!balance("((()()".toList))
  }      
  
  test("Counting is not enough - part 6.") {
    assert(!balance("(".toList))    
  }      
  
  test("Counting is not enough - part 7.") {
    assert(!balance(")".toList))
  }      
  
  test("Counting is not enough - part 8.") {
    assert(!balance(")(".toList))
  }      
  
  test("Counting is not enough - part 9.") {
    assert(!balance(")()".toList))
  }      
  
  test("Counting is not enough - part 10.") {
    assert(!balance(")()(".toList))
  }      
  
  test("Counting is not enough - part 11.") {
    assert(!balance("(()".toList))
  }        
  
  test("Counting is not enough - part 12.") {
    assert(balance("(())".toList))
  }    
}
