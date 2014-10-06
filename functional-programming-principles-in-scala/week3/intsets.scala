object intsets {
	val set1 = new NonEmpty(3, new Empty, new Empty)
	val set2 = new NonEmpty(2, new Empty, new Empty)

	val set3 = set1.include(4)               
	set3.union(set2)                       
}

abstract class IntSet {
	def include(x: Int): IntSet
	def contains(x: Int): Boolean
	def union(other: IntSet): IntSet
}

// It can be also an object
class Empty extends IntSet {
	def include(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty)
	def contains(x: Int): Boolean = false
	def union(other: IntSet): IntSet = other
	
	override def toString() = "."
}

class NonEmpty(element: Int, left: IntSet, right: IntSet) extends IntSet {
	def include(x: Int): IntSet =
		if (x < element) new NonEmpty(element, left include x, right)
		else if (x > element) new NonEmpty(element, left, right include x)
		else this

	def contains(x: Int): Boolean =
		if (x < element) left contains x
		else if (x > element) right contains x
		else true
		
	def union(other: IntSet): IntSet =
		((left union right) union other) include element
		
	override def toString() = "{" + left + element + right + "}"
}
