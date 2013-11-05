package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("minimum from one element inside the heap") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }
  
  property("minimum from two elements inserted to the heap") = forAll { (a: Int, b: Int) =>
    val heap = insert(a, empty)
    val result = insert(b, heap)
    
    (findMin(result) == a && a < b) || (findMin(result) == b && b <= a)
  }
  
  property("deleting minimum from the heap with one element should return empty heap") = forAll { a: Int =>
    val heap = insert(a, empty)
    val result = deleteMin(heap)
    
    isEmpty(result)
  }
  
  property("after melding two heaps minimum should be taken from proper one") = forAll { (a: H, b: H) =>
    val minA = findMin(a)
    val minB = findMin(b)
    val result = meld(a, b)
    
    (findMin(result) == minA && minA < minB) || (findMin(result) == minB && minB <= minA)
  }
  
  property("after deleting minimum it should have that value") = forAll { (a: Int) =>
    val heap = if (a != Int.MinValue)
        	     insert(a, insert(Int.MinValue, empty))
        	   else
                 insert(a, insert(Int.MaxValue, empty))
    
    val previousMin = findMin(heap)
    val result = deleteMin(heap)
    
    findMin(result) != previousMin
  }
  
  property("after melding and inserting new minimum it should find the newest one") = forAll { (a: H, b: H) =>
    val globalMinimum = Int.MinValue
    val heap = meld(a, b)
    val result = insert(globalMinimum, heap)
    
    findMin(result) == globalMinimum
  }
  
  property("after melding, deleting and inserting new minimum it should find global minimum") = forAll { (a: H, b: H) =>
    val globalMinimum = Int.MinValue
    
    val heap = meld(a, b)
    val changedHeap = insert(globalMinimum, heap)
    val anotherHeap = deleteMin(changedHeap)
    val result = insert(globalMinimum, heap)
    
    findMin(result) == globalMinimum
  }
  
  property("inserting another minimum to the heap shouldn't change anything") = forAll { a: H =>
  	val min = if (isEmpty(a)) 0 else findMin(a)
  	
  	findMin(insert(min, a)) == min
  }
  
  property("it should find minimum after deletion as well") = forAll { a: H =>
  	val previousMin = findMin(a)
    val heap = deleteMin(a)
  	val result = insert(previousMin, heap)
  	
  	findMin(result) == previousMin
  }
 
  property("recursively deleting minimums from any heap should give ordered collection") = forAll { a: H =>
    
  	def heapToList(heap: H, result: List[Int]): List[Int] = isEmpty(heap) match {
  	  case true => result
  	  case false => {
  	    val min = findMin(heap)
  	    val heapAfterMinimumDeletion = deleteMin(heap)
  	    
  	    heapToList(heapAfterMinimumDeletion, result ::: List(min))
  	  }  
  	} 
  	
  	def isSorted(list: List[Int]) =
  	  list.view.zip(list.tail).forall(x => x._1 <= x._2)
  
  	isSorted(heapToList(a, List()))
  }
  
  property("fiddling with heaps in order to receive the two identical heaps") = forAll { (a: H, b: H) =>    
    val melded = meld(a, b)
    
    val minA = findMin(a)
    val aWithoutMin = deleteMin(a)
    
    val bWithMinFromA = insert(minA, b)
    val identicalWithMelded = meld(aWithoutMin, bWithMinFromA)
    
    def heapToList(heap: H, result: List[Int]): List[Int] = isEmpty(heap) match {
  	  case true => result
  	  case false => {
  	    val min = findMin(heap)
  	    val heapAfterMinimumDeletion = deleteMin(heap)
  	    
  	    heapToList(heapAfterMinimumDeletion, result ::: List(min))
  	  }  
  	} 
    
    heapToList(melded, List()) == heapToList(identicalWithMelded, List())
  }

  lazy val genHeap: Gen[H] = for {
    value <- arbitrary[Int]
    heap <- oneOf(empty, genHeap)
  } yield insert(value, heap)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)
}