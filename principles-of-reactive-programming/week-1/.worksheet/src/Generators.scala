object Generators {
  
  trait Tree
  
  case class Inner(left: Tree, right: Tree) extends Tree
  case class Leaf(x: Int) extends Tree
  
  trait Generator[+T] {
  	self =>
  	
  	def generate: T
  	
  	def map[S](f: T => S): Generator[S] = new Generator[S] {
  		def generate = f(self.generate)
  	}
  	
  	def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
  		def generate = f(self.generate).generate
  	}
  };import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(523); 
 
  val integers = new Generator[Int] {
  	def generate = scala.util.Random.nextInt()
  };System.out.println("""integers  : Generators.Generator[Int] = """ + $show(integers ));$skip(52); 
  
  val booleans = for (x <- integers) yield x > 0;System.out.println("""booleans  : Generators.Generator[Boolean] = """ + $show(booleans ));$skip(84); 
  
  def single[T](x: T): Generator[T] = new Generator[T] {
  	def generate = x
  };System.out.println("""single: [T](x: T)Generators.Generator[T]""");$skip(107); 
  
  def choose(low: Int, high: Int): Generator[Int] =
  	for (x <- integers) yield low + x % (high - low);System.out.println("""choose: (low: Int, high: Int)Generators.Generator[Int]""");$skip(93); 
  
  def oneOf[T](xs: T*): Generator[T] =
  	for (idx <- choose(0, xs.length)) yield xs(idx);System.out.println("""oneOf: [T](xs: T*)Generators.Generator[T]""");$skip(135); 

  def lists: Generator[List[Int]] = for {
  	isEmpty <- booleans
  	list <- if (isEmpty) emptyLists else nonEmptyLists
  } yield list;System.out.println("""lists: => Generators.Generator[List[Int]]""");$skip(35); 
  	
  def emptyLists = single(Nil);System.out.println("""emptyLists: => Generators.Generator[scala.collection.immutable.Nil.type]""");$skip(91); 
  
  def nonEmptyLists = for {
  	head <- integers
  	tail <- lists
  } yield head :: tail;System.out.println("""nonEmptyLists: => Generators.Generator[List[Int]]""");$skip(77); 
    
  def leafs: Generator[Leaf] = for {
  	x <- integers
  } yield Leaf(x);System.out.println("""leafs: => Generators.Generator[Generators.Leaf]""");$skip(118); 
  
  def trees: Generator[Tree] = for {
  	isLeaf <- booleans
  	tree <- if (isLeaf) leafs else inners
  } yield tree;System.out.println("""trees: => Generators.Generator[Generators.Tree]""");$skip(92); 
  
  def inners: Generator[Inner] = for {
  	l <- trees
  	r <- trees
  } yield Inner(l, r);System.out.println("""inners: => Generators.Generator[Generators.Inner]""");$skip(20); val res$0 = 
  
  trees.generate;System.out.println("""res0: Generators.Tree = """ + $show(res$0))}
}
