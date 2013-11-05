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
  }
 
  val integers = new Generator[Int] {
  	def generate = scala.util.Random.nextInt()
  }                                               //> integers  : Generators.Generator[Int] = Generators$$anonfun$main$1$$anon$3@1
                                                  //| 611e9b5
  
  val booleans = for (x <- integers) yield x > 0  //> booleans  : Generators.Generator[Boolean] = Generators$Generator$$anon$1@2b7
                                                  //| ac7ba
  
  def single[T](x: T): Generator[T] = new Generator[T] {
  	def generate = x
  }                                               //> single: [T](x: T)Generators.Generator[T]
  
  def choose(low: Int, high: Int): Generator[Int] =
  	for (x <- integers) yield low + x % (high - low)
                                                  //> choose: (low: Int, high: Int)Generators.Generator[Int]
  
  def oneOf[T](xs: T*): Generator[T] =
  	for (idx <- choose(0, xs.length)) yield xs(idx)
                                                  //> oneOf: [T](xs: T*)Generators.Generator[T]

  def lists: Generator[List[Int]] = for {
  	isEmpty <- booleans
  	list <- if (isEmpty) emptyLists else nonEmptyLists
  } yield list                                    //> lists: => Generators.Generator[List[Int]]
  	
  def emptyLists = single(Nil)                    //> emptyLists: => Generators.Generator[scala.collection.immutable.Nil.type]
  
  def nonEmptyLists = for {
  	head <- integers
  	tail <- lists
  } yield head :: tail                            //> nonEmptyLists: => Generators.Generator[List[Int]]
    
  def leafs: Generator[Leaf] = for {
  	x <- integers
  } yield Leaf(x)                                 //> leafs: => Generators.Generator[Generators.Leaf]
  
  def trees: Generator[Tree] = for {
  	isLeaf <- booleans
  	tree <- if (isLeaf) leafs else inners
  } yield tree                                    //> trees: => Generators.Generator[Generators.Tree]
  
  def inners: Generator[Inner] = for {
  	l <- trees
  	r <- trees
  } yield Inner(l, r)                             //> inners: => Generators.Generator[Generators.Inner]
  
  trees.generate                                  //> res0: Generators.Tree = Inner(Leaf(176578042),Leaf(-602075090))
}